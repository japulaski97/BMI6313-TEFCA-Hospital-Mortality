# 05_primary_model.R
# Purpose: Fit primary Hybrid HWM regression model using OLS with state and year fixed effects.

library(tidyverse)
library(here)
library(broom)
library(gt)

# Load primary analytic dataset
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
  )

# Keep complete cases for variables used in the primary model
primary_model_data <- primary_hybrid_hwm %>%
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

# Check analytic sample
cat("\nPrimary model sample size:", nrow(primary_model_data), "\n")

cat("\nTEFCA status distribution:\n")
print(table(primary_model_data$tefca_status, useNA = "ifany"))

# Primary model:
# Planned TEFCA is the reference category.
# State and ONC survey year are included as fixed effects.
primary_model <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate +
    year,
  data = primary_model_data
)

# Conventional OLS coefficient table with 95% CIs
primary_results <- broom::tidy(
  primary_model,
  conf.int = TRUE,
  conf.level = 0.95
)

# Keep main coefficients for manuscript table
table2_primary <- primary_results %>%
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
    predictor = recode(
      term,
      "tefca_statusCurrent TEFCA" = "Current TEFCA vs planned TEFCA",
      "tefca_statusNeither current nor planned" = "Neither current nor planned vs planned TEFCA",
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
    Predictor = predictor,
    Estimate = estimate,
    SE = std.error,
    `95% CI`,
    `p-value` = p.value
  )

# Model fit summary
model_fit <- broom::glance(primary_model) %>%
  transmute(
    n = nobs,
    r_squared = round(r.squared, 3),
    adjusted_r_squared = round(adj.r.squared, 3),
    residual_df = df.residual
  )

# Leverage diagnostics for transparency; not necessarily for manuscript
leverage_diagnostics <- primary_model_data %>%
  mutate(hat_value = hatvalues(primary_model)) %>%
  arrange(desc(hat_value)) %>%
  select(
    facility_id,
    facility_name,
    mstate,
    year,
    tefca_status,
    score,
    denominator,
    hat_value
  )

state_counts <- primary_model_data %>%
  count(mstate, sort = TRUE) %>%
  arrange(n)

# Save outputs
dir.create(here("outputs", "models"), showWarnings = FALSE)

write_csv(
  table2_primary,
  here("outputs", "tables", "table2_primary_hybrid_hwm_model.csv")
)

write_csv(
  primary_results,
  here("outputs", "tables", "table2_primary_hybrid_hwm_all_coefficients.csv")
)

write_csv(
  model_fit,
  here("outputs", "tables", "table2_primary_model_fit.csv")
)

write_csv(
  leverage_diagnostics,
  here("outputs", "tables", "primary_model_leverage_diagnostics.csv")
)

write_csv(
  state_counts,
  here("outputs", "tables", "primary_model_state_counts.csv")
)

saveRDS(
  primary_model,
  here("outputs", "models", "primary_hybrid_hwm_model.rds")
)

# HTML table for copying/checking
table2_gt <- table2_primary %>%
  gt() %>%
  tab_header(
    title = "Table A2",
    subtitle = "Primary Linear Regression Model Predicting Hybrid HWM Score"
  ) %>%
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(2),
    column_labels.padding = px(3),
    heading.padding = px(3),
    table.width = pct(100)
  ) %>%
  cols_align(
    align = "left",
    columns = Predictor
  ) %>%
  cols_align(
    align = "center",
    columns = c(Estimate, SE, `95% CI`, `p-value`)
  ) %>%
  tab_source_note(
    source_note = "Planned-but-not-current TEFCA participation was the reference category. The model adjusted for national network participation, EHR vendor-network participation, HIO participation, log CMS denominator size, state fixed effects, and ONC survey-year fixed effects. State and year fixed effects are not shown. Higher Hybrid HWM scores indicate higher risk-standardized mortality. N = 691."
  )

gtsave(
  table2_gt,
  here("outputs", "tables", "table2_primary_hybrid_hwm_model.html")
)

cat("\nTable 2 primary model preview:\n")
print(table2_primary)

cat("\nModel fit:\n")
print(model_fit)

cat("\nHighest leverage observations:\n")
print(head(leverage_diagnostics, 10))

cat("\nStates with smallest sample sizes:\n")
print(head(state_counts, 10))