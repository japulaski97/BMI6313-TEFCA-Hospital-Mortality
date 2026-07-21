# 08_sensitivity_analyses_revised.R
# Purpose:
#   1. Conduct the primary current-vs-planned sensitivity analyses.
#   2. Read the singleton-state-excluded HC3 results produced by scripts 05 and 06.
#   3. Create one clean, two-panel Table A4 with gt.
#   4. Preserve Figure A4 and the fitted-model outputs.

library(tidyverse)
library(here)
library(broom)
library(gt)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "models"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Primary analytic dataset
# ------------------------------------------------------------------------------

primary_hybrid_hwm <- read_csv(
  here("data_processed", "primary_hybrid_hwm.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    tefca_status = factor(
      tefca_status,
      levels = c(
        "Planned TEFCA",
        "Current TEFCA",
        "Neither current nor planned"
      )
    ),
    mstate = factor(mstate),
    year = factor(year)
  ) %>%
  filter(
    !is.na(score),
    !is.na(tefca_status),
    !is.na(national_network),
    !is.na(vendor_network),
    !is.na(hio),
    !is.na(log_denominator),
    is.finite(log_denominator),
    !is.na(mstate),
    !is.na(year)
  )

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

extract_current_vs_planned <- function(fitted_model, model_name) {
  model_fit <- broom::glance(fitted_model)

  broom::tidy(
    fitted_model,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    filter(term == "tefca_statusCurrent TEFCA") %>%
    transmute(
      analysis = model_name,
      n = stats::nobs(fitted_model),
      estimate,
      se = std.error,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value,
      r_squared = model_fit$r.squared,
      adjusted_r_squared = model_fit$adj.r.squared
    )
}

read_hc3_current_vs_planned <- function(
    results_file,
    summary_file,
    outcome_label
) {
  if (!file.exists(results_file)) {
    stop(
      "Missing HC3 results file: ", results_file,
      "\nRun scripts 05_primary_model.R and 06_secondary_models.R first."
    )
  }

  if (!file.exists(summary_file)) {
    stop(
      "Missing singleton-state summary file: ", summary_file,
      "\nRun scripts 05_primary_model.R and 06_secondary_models.R first."
    )
  }

  hc3_result <- read_csv(results_file, show_col_types = FALSE) %>%
    filter(term == "tefca_statusCurrent TEFCA")

  sensitivity_summary <- read_csv(
    summary_file,
    show_col_types = FALSE
  )

  if (nrow(hc3_result) != 1) {
    stop(
      "Expected exactly one current-vs-planned row in: ",
      results_file
    )
  }

  tibble(
    analysis = outcome_label,
    n = sensitivity_summary$sensitivity_n[[1]],
    estimate = hc3_result$estimate[[1]],
    se = hc3_result$hc3_se[[1]],
    conf_low = hc3_result$hc3_conf_low[[1]],
    conf_high = hc3_result$hc3_conf_high[[1]],
    p_value = hc3_result$hc3_p_value[[1]],
    r_squared = NA_real_,
    adjusted_r_squared = NA_real_
  )
}

format_number <- function(x) {
  out <- sprintf("%.3f", x)
  out <- stringr::str_replace_all(out, "-", "\u2212")
  out
}

format_p_value <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "< .001**",
    p < 0.05 ~ paste0(
      stringr::str_remove(sprintf("%.3f", p), "^0"),
      "*"
    ),
    TRUE ~ stringr::str_remove(sprintf("%.3f", p), "^0")
  )
}

read_excluded_states <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  read_csv(path, show_col_types = FALSE) %>%
    pull(mstate) %>%
    as.character()
}

# ------------------------------------------------------------------------------
# Panel A models
# ------------------------------------------------------------------------------

main_model <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate +
    year,
  data = primary_hybrid_hwm
)

model_2024 <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate,
  data = primary_hybrid_hwm %>%
    filter(year == "2024") %>%
    droplevels()
)

wls_model <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate +
    year,
  data = primary_hybrid_hwm,
  weights = denominator
)

current_planned_only_model <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate +
    year,
  data = primary_hybrid_hwm %>%
    filter(
      tefca_status %in% c(
        "Planned TEFCA",
        "Current TEFCA"
      )
    ) %>%
    droplevels()
)

panel_a <- bind_rows(
  extract_current_vs_planned(main_model, "Main OLS model"),
  extract_current_vs_planned(model_2024, "2024-only model"),
  extract_current_vs_planned(wls_model, "Weighted least squares model"),
  extract_current_vs_planned(
    current_planned_only_model,
    "Current/planned-only model"
  )
) %>%
  mutate(
    panel = "Panel A. Primary-Outcome Sensitivity Models",
    analysis = factor(
      analysis,
      levels = c(
        "Main OLS model",
        "2024-only model",
        "Weighted least squares model",
        "Current/planned-only model"
      )
    )
  ) %>%
  arrange(analysis) %>%
  mutate(analysis = as.character(analysis))

# ------------------------------------------------------------------------------
# Panel B: singleton-state-excluded HC3 analyses
# ------------------------------------------------------------------------------

panel_b <- bind_rows(
  read_hc3_current_vs_planned(
    here(
      "outputs", "diagnostics",
      "primary_hybrid_hwm_singleton_states_excluded_hc3.csv"
    ),
    here(
      "outputs", "diagnostics",
      "primary_hybrid_hwm_singleton_state_sensitivity_summary.csv"
    ),
    "Hybrid HWM"
  ),
  read_hc3_current_vs_planned(
    here(
      "outputs", "diagnostics",
      "secondary_heart_failure_singleton_states_excluded_hc3.csv"
    ),
    here(
      "outputs", "diagnostics",
      "secondary_heart_failure_singleton_state_sensitivity_summary.csv"
    ),
    "Heart failure mortality"
  ),
  read_hc3_current_vs_planned(
    here(
      "outputs", "diagnostics",
      "secondary_pneumonia_singleton_states_excluded_hc3.csv"
    ),
    here(
      "outputs", "diagnostics",
      "secondary_pneumonia_singleton_state_sensitivity_summary.csv"
    ),
    "Pneumonia mortality"
  )
) %>%
  mutate(
    panel =
      "Panel B. HC3 Robust-Standard-Error Analyses Excluding Singleton-State Observations"
  )

# ------------------------------------------------------------------------------
# Combine and format Table A4
# ------------------------------------------------------------------------------

table_a4_data <- bind_rows(panel_a, panel_b) %>%
  mutate(
    Estimate = format_number(estimate),
    SE = format_number(se),
    `95% CI` = paste0(
      "[",
      format_number(conf_low),
      ", ",
      format_number(conf_high),
      "]"
    ),
    `p-value` = format_p_value(p_value),
    `R-squared` = if_else(
      is.na(r_squared),
      "",
      format_number(r_squared)
    ),
    `Adjusted R-squared` = if_else(
      is.na(adjusted_r_squared),
      "",
      format_number(adjusted_r_squared)
    )
  ) %>%
  transmute(
    Panel = panel,
    Analysis = analysis,
    N = n,
    Estimate,
    SE,
    `95% CI`,
    `p-value`,
    `R-squared`,
    `Adjusted R-squared`
  )

# Save a plain CSV copy.
write_csv(
  table_a4_data %>% select(-Panel),
  here(
    "outputs", "tables",
    "tableA4_sensitivity_current_vs_planned.csv"
  )
)

# Dynamically record the excluded states for the table note.
primary_excluded <- read_excluded_states(
  here(
    "outputs", "diagnostics",
    "primary_hybrid_hwm_excluded_singleton_states.csv"
  )
)

hf_excluded <- read_excluded_states(
  here(
    "outputs", "diagnostics",
    "secondary_heart_failure_excluded_singleton_states.csv"
  )
)

pn_excluded <- read_excluded_states(
  here(
    "outputs", "diagnostics",
    "secondary_pneumonia_excluded_singleton_states.csv"
  )
)

excluded_state_note <- paste0(
  "The excluded states were ",
  paste(primary_excluded, collapse = ", "),
  " for Hybrid HWM; ",
  paste(hf_excluded, collapse = ", "),
  " for heart failure mortality; and ",
  paste(pn_excluded, collapse = ", "),
  " for pneumonia mortality."
)

table_note <- paste(
  "Estimates are adjusted coefficients for current TEFCA participation relative to planned TEFCA participation.",
  "Positive estimates indicate higher mortality scores among current-participating hospitals.",
  "All models adjusted for national-network, EHR vendor-network, and state/regional/local HIO participation, log CMS denominator, state, and ONC survey year, except that the 2024-only model omitted survey-year fixed effects.",
  "CMS denominator refers to the number of eligible hospital cases or admissions included in the relevant measure.",
  "The weighted least squares model used the untransformed CMS denominator as the analytic weight, and the current/planned-only model excluded hospitals with neither current nor planned TEFCA participation.",
  "Panel B reports HC3 heteroskedasticity-robust standard errors after excluding states represented by one hospital in the relevant analytic sample.",
  excluded_state_note,
  "*p* < .05. **p** < .001."
)

table_a4_gt <- table_a4_data %>%
  gt(
    groupname_col = "Panel"
  ) %>%
  tab_header(
    title = md("**Table A4**"),
    subtitle = md(
      "*Sensitivity Analyses for the Current TEFCA Versus Planned TEFCA Comparison*"
    )
  ) %>%
  cols_label(
    Analysis = "Model or outcome",
    N = "N",
    Estimate = "Estimate",
    SE = "SE",
    `95% CI` = "95% CI",
    `p-value` = md("*p*-value"),
    `R-squared` = html("R<sup>2</sup>"),
    `Adjusted R-squared` = html("Adjusted R<sup>2</sup>")
  ) %>%
  cols_align(
    align = "left",
    columns = Analysis
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      N,
      Estimate,
      SE,
      `95% CI`,
      `p-value`,
      `R-squared`,
      `Adjusted R-squared`
    )
  ) %>%
  cols_width(
    Analysis ~ px(235),
    N ~ px(60),
    Estimate ~ px(82),
    SE ~ px(72),
    `95% CI` ~ px(145),
    `p-value` ~ px(78),
    `R-squared` ~ px(72),
    `Adjusted R-squared` ~ px(105)
  ) %>%
  tab_source_note(
    source_note = md(paste0("**Note.** ", table_note))
  ) %>%
  tab_options(
    table.width = px(900),
    table.border.top.width = px(0),
    table.border.bottom.width = px(1),
    table.border.bottom.color = "#666666",
    heading.border.bottom.width = px(0),
    column_labels.border.top.width = px(0),
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#777777",
    row_group.border.top.width = px(1),
    row_group.border.top.color = "#777777",
    row_group.border.bottom.width = px(0),
    table_body.hlines.width = px(0),
    data_row.padding = px(7),
    row_group.padding = px(8),
    source_notes.padding = px(10)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  opt_table_font(
    font = list(
      "Times New Roman",
      "Times",
      "serif"
    )
  )

gtsave(
  table_a4_gt,
  here(
    "outputs", "tables",
    "tableA4_sensitivity_current_vs_planned.html"
  )
)

# Optional PNG output when webshot2 is installed.
if (requireNamespace("webshot2", quietly = TRUE)) {
  gtsave(
    table_a4_gt,
    here(
      "outputs", "tables",
      "tableA4_sensitivity_current_vs_planned.png"
    ),
    zoom = 2
  )
} else {
  message(
    "Package 'webshot2' is not installed, so only the HTML table was saved. ",
    "Open the HTML file in a browser and copy the table into Google Docs."
  )
}

# ------------------------------------------------------------------------------
# Preserve Figure A4
# ------------------------------------------------------------------------------

figure_a4_results <- panel_a %>%
  mutate(
    analysis = factor(
      analysis,
      levels = rev(c(
        "Main OLS model",
        "2024-only model",
        "Weighted least squares model",
        "Current/planned-only model"
      ))
    )
  )

figureA4 <- ggplot(
  figure_a4_results,
  aes(
    x = analysis,
    y = estimate,
    ymin = conf_low,
    ymax = conf_high
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_pointrange() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Adjusted coefficient estimate"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  here(
    "outputs", "figures",
    "figureA4_sensitivity_current_vs_planned.png"
  ),
  figureA4,
  width = 7,
  height = 4,
  dpi = 300
)

ggsave(
  here(
    "outputs", "figures",
    "figureA4_sensitivity_current_vs_planned.pdf"
  ),
  figureA4,
  width = 7,
  height = 4
)

# ------------------------------------------------------------------------------
# Save fitted models
# ------------------------------------------------------------------------------

saveRDS(
  main_model,
  here("outputs", "models", "sensitivity_main_ols_model.rds")
)
saveRDS(
  model_2024,
  here("outputs", "models", "sensitivity_2024_only_model.rds")
)
saveRDS(
  wls_model,
  here("outputs", "models", "sensitivity_wls_model.rds")
)
saveRDS(
  current_planned_only_model,
  here(
    "outputs", "models",
    "sensitivity_current_planned_only_model.rds"
  )
)

cat("\nTable A4 preview:\n")
print(table_a4_data)

cat(
  "\nSaved the revised Table A4 HTML",
  if (requireNamespace("webshot2", quietly = TRUE)) " and PNG" else "",
  ", along with Figure A4 outputs.\n",
  sep = ""
)
