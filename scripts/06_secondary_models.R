# 06_secondary_models.R
# Purpose: Fit secondary mortality regression models for heart failure and pneumonia.

library(tidyverse)
library(here)
library(broom)
library(gt)

source(here("scripts", "10_model_diagnostics_helpers.R"))
source(here("scripts", "11_singleton_state_sensitivity.R"))

secondary_mortality <- read_csv(
  here("data_processed", "secondary_mortality.csv"),
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
  )

# Function to fit one outcome model
fit_secondary_model <- function(data, outcome_id) {
  model_data <- data %>%
    filter(measure_id == outcome_id) %>%
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

  model <- lm(
    score ~ tefca_status +
      national_network +
      vendor_network +
      hio +
      log_denominator +
      mstate +
      year,
    data = model_data
  )

  results <- tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(
      measure_id = outcome_id,
      n = nobs(model),
      r_squared = glance(model)$r.squared,
      adjusted_r_squared = glance(model)$adj.r.squared
    )

  list(
    model = model,
    data = model_data,
    results = results
  )
}

hf <- fit_secondary_model(secondary_mortality, "MORT_30_HF")
pn <- fit_secondary_model(secondary_mortality, "MORT_30_PN")

secondary_results_all <- bind_rows(
  hf$results,
  pn$results
)

# Keep manuscript-relevant coefficients
tableA3_secondary <- secondary_results_all %>%
  filter(
    term %in% c(
      "tefca_statusCurrent TEFCA",
      "tefca_statusNeither current nor planned",
      "national_network",
      "vendor_network",
      "hio",
      "log_denominator"
    )
  ) %>%
  mutate(
    outcome = recode(
      measure_id,
      "MORT_30_HF" = "Heart failure mortality",
      "MORT_30_PN" = "Pneumonia mortality"
    ),
    predictor = recode(
      term,
      "tefca_statusCurrent TEFCA" = "Current TEFCA vs planned TEFCA",
      "tefca_statusNeither current nor planned" = "Neither vs planned TEFCA",
      "national_network" = "National network participation",
      "vendor_network" = "EHR vendor-network participation",
      "hio" = "State/regional/local HIO participation",
      "log_denominator" = "Log CMS denominator"
    ),
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = if_else(
      p.value < 0.001,
      "<0.001",
      as.character(round(p.value, 3))
    ),
    `95% CI` = paste0("[", conf.low, ", ", conf.high, "]")
  ) %>%
  select(
    Outcome = outcome,
    Predictor = predictor,
    Estimate = estimate,
    SE = std.error,
    `95% CI`,
    `p-value` = p.value
  )

secondary_model_fit <- secondary_results_all %>%
  distinct(measure_id, n, r_squared, adjusted_r_squared) %>%
  mutate(
    outcome = recode(
      measure_id,
      "MORT_30_HF" = "Heart failure mortality",
      "MORT_30_PN" = "Pneumonia mortality"
    ),
    r_squared = round(r_squared, 3),
    adjusted_r_squared = round(adjusted_r_squared, 3)
  ) %>%
  select(
    Outcome = outcome,
    N = n,
    `R-squared` = r_squared,
    `Adjusted R-squared` = adjusted_r_squared
  )

dir.create(here("outputs", "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(
  tableA3_secondary,
  here("outputs", "tables", "tableA3_secondary_mortality_models.csv")
)

write_csv(
  secondary_model_fit,
  here("outputs", "tables", "tableA3_secondary_model_fit.csv")
)

saveRDS(hf$model, here("outputs", "models", "secondary_hf_model.rds"))
saveRDS(pn$model, here("outputs", "models", "secondary_pn_model.rds"))

tableA3_gt <- tableA3_secondary %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(
    title = "Table A3",
    subtitle = "Secondary linear regression models predicting heart failure and pneumonia mortality scores"
  ) %>%
  tab_source_note(
    source_note = "Planned-but-not-current TEFCA participation was the reference category. Each model adjusted for national network participation, EHR vendor-network participation, HIO participation, log CMS denominator size, state fixed effects, and ONC survey-year fixed effects. State and year fixed effects are not shown."
  )

gtsave(
  tableA3_gt,
  here("outputs", "tables", "tableA3_secondary_mortality_models.html")
)

cat("\nTable A3 secondary model preview:\n")
print(tableA3_secondary, n = Inf)

cat("\nSecondary model fit:\n")
print(secondary_model_fit)

# Model diagnostics and HC1 robust-standard-error sensitivity checks
secondary_key_terms <- c(
  "tefca_statusCurrent TEFCA",
  "tefca_statusNeither current nor planned",
  "national_network",
  "vendor_network",
  "hio",
  "log_denominator"
)

hf_diagnostics <- run_lm_diagnostics(
  model = hf$model,
  model_data = hf$data,
  model_id = "secondary_heart_failure",
  key_terms = secondary_key_terms
)

pn_diagnostics <- run_lm_diagnostics(
  model = pn$model,
  model_data = pn$data,
  model_id = "secondary_pneumonia",
  key_terms = secondary_key_terms
)

secondary_diagnostic_summary <- bind_rows(
  hf_diagnostics$summary,
  pn_diagnostics$summary
)

write_csv(
  secondary_diagnostic_summary,
  here("outputs", "diagnostics", "secondary_models_diagnostic_summary.csv")
)

cat("\nSecondary diagnostic summaries:\n")
print(secondary_diagnostic_summary)

cat("\nHeart failure key coefficients: conventional vs HC1 robust SEs:\n")
print(hf_diagnostics$key_conventional_vs_hc1)

cat("\nPneumonia key coefficients: conventional vs HC1 robust SEs:\n")
print(pn_diagnostics$key_conventional_vs_hc1)

# Sensitivity analyses excluding states represented by one hospital
hf_singleton_sensitivity <-
  run_singleton_state_sensitivity(
    original_model = hf$model,
    model_data = hf$data,
    model_id = "secondary_heart_failure"
  )

pn_singleton_sensitivity <-
  run_singleton_state_sensitivity(
    original_model = pn$model,
    model_data = pn$data,
    model_id = "secondary_pneumonia"
  )
