# 10_model_diagnostics_helpers.R
# Purpose: Reusable OLS diagnostics and HC1 robust-standard-error sensitivity outputs.

required_packages <- c("lmtest", "sandwich", "car")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages before running diagnostics: ",
      paste(missing_packages, collapse = ", "),
      ". In this renv project, run: renv::install(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    )
  )
}

run_lm_diagnostics <- function(
    model,
    model_data,
    model_id,
    key_terms = NULL
) {
  diagnostics_dir <- here::here("outputs", "diagnostics")
  diagnostic_figures_dir <- here::here("outputs", "figures", "diagnostics")

  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(diagnostic_figures_dir, recursive = TRUE, showWarnings = FALSE)

  if (!inherits(model, "lm")) {
    stop("model must be an object fitted with lm().")
  }

  if (nrow(model_data) != stats::nobs(model)) {
    stop(
      "model_data does not have the same number of rows as the fitted model. ",
      "Fit the model on the supplied complete-case dataset before running diagnostics."
    )
  }
  
  n <- stats::nobs(model)
  model_rank <- model$rank
  residual_df_value <- stats::df.residual(model)
  cooks_cutoff <- 4 / n
  leverage_cutoff <- 2 * model_rank / n

  # Breusch-Pagan test for unequal residual variance.
  bp_test <- lmtest::bptest(model)

  # HC1 is used instead of HC3 because one or more observations have hat values
  # equal or extremely close to 1 under the state fixed-effects specification.
  # HC3 divides by powers of (1 - hat value) and is therefore undefined in that case.
  hc1_vcov <- sandwich::vcovHC(model, type = "HC1")
  hc1_test <- lmtest::coeftest(model, vcov. = hc1_vcov)
  hc1_critical <- stats::qt(0.975, df = residual_df_value)

  robust_results <- tibble::tibble(
    term = rownames(hc1_test),
    estimate = unname(hc1_test[, 1]),
    robust_se_hc1 = unname(hc1_test[, 2]),
    statistic_hc1 = unname(hc1_test[, 3]),
    robust_p_value_hc1 = unname(hc1_test[, 4]),
    robust_conf_low_hc1 = estimate - hc1_critical * robust_se_hc1,
    robust_conf_high_hc1 = estimate + hc1_critical * robust_se_hc1
  )

  conventional_results <- broom::tidy(
    model,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    dplyr::transmute(
      term,
      estimate,
      conventional_se = std.error,
      conventional_p_value = p.value,
      conventional_conf_low = conf.low,
      conventional_conf_high = conf.high
    )

  se_comparison <- conventional_results %>%
    dplyr::left_join(robust_results, by = c("term", "estimate"))

  if (!is.null(key_terms)) {
    key_se_comparison <- se_comparison %>%
      dplyr::filter(term %in% key_terms)
  } else {
    key_se_comparison <- se_comparison
  }

  # Influence and residual diagnostics.
  diagnostic_values <- tibble::tibble(
    fitted_value = stats::fitted(model),
    residual = stats::residuals(model),
    studentized_residual = stats::rstudent(model),
    hat_value = stats::hatvalues(model),
    cooks_distance = stats::cooks.distance(model)
  )

  identifying_columns <- intersect(
    c(
      "facility_id", "facility_name", "measure_id", "mstate", "year",
      "tefca_status", "score", "denominator"
    ),
    names(model_data)
  )

  influence_table <- dplyr::bind_cols(
    model_data %>% dplyr::select(dplyr::all_of(identifying_columns)),
    diagnostic_values
  ) %>%
    dplyr::mutate(
      cooks_flag = cooks_distance > cooks_cutoff,
      leverage_flag = hat_value > leverage_cutoff,
      hat_near_one_flag = hat_value >= 0.999999,
      studentized_residual_flag = abs(studentized_residual) > 3
    ) %>%
    dplyr::arrange(dplyr::desc(cooks_distance))

  # Use "model_name" rather than "model" here. tibble evaluates columns
  # sequentially, so naming the first column "model" would mask the lm object.
  diagnostic_summary <- tibble::tibble(
    model_name = model_id,
    n = n,
    model_rank = model_rank,
    residual_df = residual_df_value,
    breusch_pagan_statistic = unname(bp_test$statistic),
    breusch_pagan_df = unname(bp_test$parameter),
    breusch_pagan_p_value = bp_test$p.value,
    cooks_distance_cutoff = cooks_cutoff,
    n_cooks_above_cutoff = sum(influence_table$cooks_flag, na.rm = TRUE),
    max_cooks_distance = max(influence_table$cooks_distance, na.rm = TRUE),
    leverage_cutoff = leverage_cutoff,
    n_leverage_above_cutoff = sum(influence_table$leverage_flag, na.rm = TRUE),
    n_hat_near_one = sum(influence_table$hat_near_one_flag, na.rm = TRUE),
    max_hat_value = max(influence_table$hat_value, na.rm = TRUE),
    n_abs_studentized_residual_above_3 =
      sum(influence_table$studentized_residual_flag, na.rm = TRUE),
    max_abs_studentized_residual =
      max(abs(influence_table$studentized_residual), na.rm = TRUE)
  )

  # Variance-inflation diagnostics. Factors with more than one degree of
  # freedom are reported using generalized VIF values.
  vif_result <- tryCatch(
    car::vif(model),
    error = function(e) e
  )

  if (inherits(vif_result, "error")) {
    vif_table <- tibble::tibble(
      term = NA_character_,
      note = conditionMessage(vif_result)
    )
  } else if (is.matrix(vif_result)) {
    vif_table <- tibble::as_tibble(
      vif_result,
      rownames = "term"
    )
  } else {
    vif_table <- tibble::tibble(
      term = names(vif_result),
      VIF = as.numeric(vif_result)
    )
  }

  # Save numerical outputs before drawing plots so that a plotting problem
  # cannot prevent the tables from being written.
  readr::write_csv(
    diagnostic_summary,
    file.path(diagnostics_dir, paste0(model_id, "_diagnostic_summary.csv"))
  )

  readr::write_csv(
    influence_table,
    file.path(diagnostics_dir, paste0(model_id, "_influence_diagnostics.csv"))
  )

  readr::write_csv(
    se_comparison,
    file.path(diagnostics_dir, paste0(model_id, "_conventional_vs_hc1.csv"))
  )

  readr::write_csv(
    key_se_comparison,
    file.path(
      diagnostics_dir,
      paste0(model_id, "_key_coefficients_conventional_vs_hc1.csv")
    )
  )

  readr::write_csv(
    vif_table,
    file.path(diagnostics_dir, paste0(model_id, "_vif.csv"))
  )

  # Save standard four-panel lm diagnostic plots.
  pdf_path <- file.path(
    diagnostic_figures_dir,
    paste0(model_id, "_diagnostic_plots.pdf")
  )
  grDevices::pdf(pdf_path, width = 8, height = 8)
  old_par <- graphics::par(mfrow = c(2, 2))
  graphics::plot(model, which = 1:4)
  graphics::par(old_par)
  grDevices::dev.off()

  png_path <- file.path(
    diagnostic_figures_dir,
    paste0(model_id, "_diagnostic_plots.png")
  )
  grDevices::png(png_path, width = 1800, height = 1800, res = 220)
  old_par <- graphics::par(mfrow = c(2, 2))
  graphics::plot(model, which = 1:4)
  graphics::par(old_par)
  grDevices::dev.off()

  list(
    summary = diagnostic_summary,
    influence = influence_table,
    conventional_vs_hc1 = se_comparison,
    key_conventional_vs_hc1 = key_se_comparison,
    vif = vif_table
  )
}
