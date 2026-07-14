# 08_sensitivity_analyses.R
# Purpose: Conduct sensitivity analyses for the current-vs-planned TEFCA estimate.

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

# Helper function to extract the current-vs-planned estimate
extract_current_vs_planned <- function(fitted_model, model_name) {
  model_fit <- broom::glance(fitted_model)
  
  broom::tidy(
    fitted_model,
    conf.int = TRUE,
    conf.level = 0.95
  ) %>%
    filter(term == "tefca_statusCurrent TEFCA") %>%
    mutate(
      model_name = model_name,
      n = stats::nobs(fitted_model),
      r_squared = model_fit$r.squared,
      adjusted_r_squared = model_fit$adj.r.squared
    ) %>%
    select(
      model_name,
      n,
      term,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value,
      r_squared,
      adjusted_r_squared
    )
}

# 1. Main OLS model: state and year fixed effects
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

# 2. 2024-only model: state fixed effects only because year is constant
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

# 3. Weighted least squares model: weighted by CMS denominator
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

# 4. Current-and-planned-only model: exclude neither-current-nor-planned hospitals
current_planned_only_model <- lm(
  score ~ tefca_status +
    national_network +
    vendor_network +
    hio +
    log_denominator +
    mstate +
    year,
  data = primary_hybrid_hwm %>%
    filter(tefca_status %in% c("Planned TEFCA", "Current TEFCA")) %>%
    droplevels()
)

# Combine sensitivity estimates
sensitivity_results <- bind_rows(
  extract_current_vs_planned(main_model, "Main OLS model"),
  extract_current_vs_planned(model_2024, "2024-only model"),
  extract_current_vs_planned(wls_model, "Weighted least squares model"),
  extract_current_vs_planned(current_planned_only_model, "Current-and-planned-only model")
) %>%
  mutate(
    model_name = factor(
      model_name,
      levels = rev(c(
        "Main OLS model",
        "2024-only model",
        "Weighted least squares model",
        "Current-and-planned-only model"
      ))
    )
  )

# Formatted Table A4
tableA4_sensitivity <- sensitivity_results %>%
  mutate(
    Estimate = round(estimate, 3),
    SE = round(std.error, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    `95% CI` = paste0("[", conf.low, ", ", conf.high, "]"),
    `p-value` = if_else(
      p.value < 0.001,
      "<0.001",
      as.character(round(p.value, 3))
    ),
    `R-squared` = round(r_squared, 3),
    `Adjusted R-squared` = round(adjusted_r_squared, 3)
  ) %>%
  transmute(
    Model = as.character(model_name),
    N = n,
    Estimate,
    SE,
    `95% CI`,
    `p-value`,
    `R-squared`,
    `Adjusted R-squared`
  )

# Save table outputs
write_csv(
  tableA4_sensitivity,
  here("outputs", "tables", "tableA4_sensitivity_current_vs_planned.csv")
)

tableA4_gt <- tableA4_sensitivity %>%
  gt() %>%
  tab_header(
    title = "Table A4",
    subtitle = "Sensitivity Analyses for the Current TEFCA Versus Planned TEFCA Estimate"
  ) %>%
  tab_source_note(
    source_note = "The estimate shown is the adjusted coefficient for current TEFCA participation relative to planned TEFCA participation. The 2024-only model omitted year fixed effects because only one ONC survey year was included."
  )

gtsave(
  tableA4_gt,
  here("outputs", "tables", "tableA4_sensitivity_current_vs_planned.html")
)

# Create Figure A4 without embedded title, subtitle, or note
figureA4 <- ggplot(
  sensitivity_results,
  aes(x = model_name, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
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
  here("outputs", "figures", "figureA4_sensitivity_current_vs_planned.png"),
  figureA4,
  width = 7,
  height = 4,
  dpi = 300
)

ggsave(
  here("outputs", "figures", "figureA4_sensitivity_current_vs_planned.pdf"),
  figureA4,
  width = 7,
  height = 4
)

# Save models
saveRDS(main_model, here("outputs", "models", "sensitivity_main_ols_model.rds"))
saveRDS(model_2024, here("outputs", "models", "sensitivity_2024_only_model.rds"))
saveRDS(wls_model, here("outputs", "models", "sensitivity_wls_model.rds"))
saveRDS(current_planned_only_model, here("outputs", "models", "sensitivity_current_planned_only_model.rds"))

cat("\nTable A4 sensitivity analysis preview:\n")
print(tableA4_sensitivity)

cat("\nSaved Table A4 and Figure A4 outputs.\n")