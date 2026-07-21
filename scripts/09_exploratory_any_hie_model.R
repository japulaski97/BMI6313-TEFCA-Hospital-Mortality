# scripts/09_exploratory_any_hie_model.R

library(tidyverse)
library(broom)

# Use the same primary analytic dataset used in 05_primary_model.R.
# Replace this object/file name if your actual saved file has a different name.
primary_df <- readr::read_csv("data_processed/primary_hybrid_hwm.csv")

# Inspect names if needed
names(primary_df)

# Create exploratory "any measured HIE" variable.
# Planned TEFCA is intentionally NOT counted as HIE participation.
primary_df <- primary_df %>%
  mutate(
    any_measured_hie = if_else(
      tefca_status == "Current" |
        national_network == 1 |
        vendor_network == 1 |
        hio == 1,
      1,
      0
    )
  )

# Check group sizes
primary_df %>%
  count(any_measured_hie)

# Check descriptive outcome means
primary_df %>%
  group_by(any_measured_hie) %>%
  summarise(
    n = n(),
    mean_hybrid_hwm = mean(score, na.rm = TRUE),
    sd_hybrid_hwm = sd(score, na.rm = TRUE),
    median_denominator = median(denominator, na.rm = TRUE),
    .groups = "drop"
  )

# Exploratory model: any measured HIE vs no measured HIE
any_hie_model <- lm(
  score ~ any_measured_hie + log_denominator + factor(cms_state) + factor(year),
  data = primary_df
)

summary(any_hie_model)

any_hie_results <- broom::tidy(any_hie_model, conf.int = TRUE) %>%
  filter(term == "any_measured_hie") %>%
  mutate(
    model = "Exploratory any measured HIE model"
  ) %>%
  select(model, term, estimate, std.error, conf.low, conf.high, p.value)

any_hie_results

readr::write_csv(
  any_hie_results,
  "outputs/tables/exploratory_any_measured_hie_model.csv"
)