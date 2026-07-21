# 11_singleton_state_sensitivity.R

run_singleton_state_sensitivity <- function(
    original_model,
    model_data,
    model_id
) {
  required_packages <- c("dplyr", "tibble", "readr", "here", "lmtest", "sandwich")

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Install the following packages first: ",
      paste(missing_packages, collapse = ", ")
    )
  }

  if (!inherits(original_model, "lm")) {
    stop("original_model must be an lm object.")
  }

  if (!"mstate" %in% names(model_data)) {
    stop("model_data must contain the state variable 'mstate'.")
  }

  diagnostics_dir <- here::here("outputs", "diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

  state_counts <- model_data |>
    dplyr::count(mstate, name = "state_n")

  singleton_states <- state_counts |>
    dplyr::filter(state_n == 1)

  sensitivity_data <- model_data |>
    dplyr::left_join(state_counts, by = "mstate") |>
    dplyr::filter(state_n > 1) |>
    dplyr::select(-state_n) |>
    droplevels()

  if ("tefca_status" %in% names(sensitivity_data) &&
      "Planned TEFCA" %in% levels(factor(sensitivity_data$tefca_status))) {
    sensitivity_data$tefca_status <- stats::relevel(
      factor(sensitivity_data$tefca_status),
      ref = "Planned TEFCA"
    )
  }

  if ("year" %in% names(sensitivity_data)) {
    sensitivity_data$year <- factor(sensitivity_data$year)
  }

  sensitivity_data$mstate <- factor(sensitivity_data$mstate)

  sensitivity_model <- stats::lm(
    stats::formula(original_model),
    data = sensitivity_data
  )

  max_hat <- max(stats::hatvalues(sensitivity_model), na.rm = TRUE)

  if (max_hat >= 0.999999) {
    warning(
      "At least one leverage value remains extremely close to 1 after ",
      "excluding singleton states. HC3 results may still be unstable."
    )
  }

  hc3_vcov <- sandwich::vcovHC(
    sensitivity_model,
    type = "HC3"
  )

  hc3_test <- lmtest::coeftest(
    sensitivity_model,
    vcov. = hc3_vcov
  )

  critical_value <- stats::qt(
    0.975,
    df = stats::df.residual(sensitivity_model)
  )

  results <- tibble::tibble(
    term = rownames(hc3_test),
    estimate = unname(hc3_test[, 1]),
    hc3_se = unname(hc3_test[, 2]),
    hc3_statistic = unname(hc3_test[, 3]),
    hc3_p_value = unname(hc3_test[, 4]),
    hc3_conf_low = estimate - critical_value * hc3_se,
    hc3_conf_high = estimate + critical_value * hc3_se
  )

  summary_table <- tibble::tibble(
    model_name = model_id,
    original_n = nrow(model_data),
    sensitivity_n = nrow(sensitivity_data),
    n_singleton_states_excluded = nrow(singleton_states),
    max_hat_after_exclusion = max_hat
  )

  readr::write_csv(
    results,
    file.path(
      diagnostics_dir,
      paste0(model_id, "_singleton_states_excluded_hc3.csv")
    )
  )

  readr::write_csv(
    singleton_states,
    file.path(
      diagnostics_dir,
      paste0(model_id, "_excluded_singleton_states.csv")
    )
  )

  readr::write_csv(
    summary_table,
    file.path(
      diagnostics_dir,
      paste0(model_id, "_singleton_state_sensitivity_summary.csv")
    )
  )

  cat("\nModel:", model_id, "\n")
  cat("Original N:", nrow(model_data), "\n")
  cat("Sensitivity N:", nrow(sensitivity_data), "\n")
  cat("Excluded singleton states:\n")
  print(singleton_states)

  key_terms <- c(
    "tefca_statusCurrent TEFCA",
    "tefca_statusNeither current nor planned",
    "national_network",
    "vendor_network",
    "hio",
    "log_denominator"
  )

  cat("\nKey HC3 results:\n")
  print(
    results |>
      dplyr::filter(term %in% key_terms)
  )

  invisible(
    list(
      data = sensitivity_data,
      model = sensitivity_model,
      results = results,
      singleton_states = singleton_states,
      summary = summary_table
    )
  )
}
