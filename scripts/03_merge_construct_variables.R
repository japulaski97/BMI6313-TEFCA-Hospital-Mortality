# 03_merge_construct_variables.R
# Purpose: Merge ONC and CMS data and create analytic datasets.

library(tidyverse)
library(here)

onc_clean <- read_csv(
  here("data_processed", "onc_clean.csv"),
  show_col_types = FALSE
)

cms_clean <- read_csv(
  here("data_processed", "cms_clean_all_measures.csv"),
  show_col_types = FALSE
)

# Keep only outcome measures used in this study
cms_outcomes <- cms_clean %>%
  filter(measure_id %in% c("Hybrid_HWM", "MORT_30_HF", "MORT_30_PN")) %>%
  select(
    facility_id,
    facility_name,
    cms_state = state,
    measure_id,
    measure_name,
    denominator,
    score,
    lower_estimate,
    higher_estimate,
    start_date,
    end_date
  )

# Primary analytic dataset: Hybrid HWM only
primary_hybrid_hwm <- cms_outcomes %>%
  filter(measure_id == "Hybrid_HWM") %>%
  filter(!is.na(score)) %>%
  inner_join(onc_clean, by = "facility_id") %>%
  mutate(
    log_denominator = log(denominator),
    tefca_status = factor(
      tefca_status,
      levels = c(
        "Planned TEFCA",
        "Current TEFCA",
        "Neither current nor planned"
      )
    )
  )

# Secondary analytic dataset: HF and pneumonia mortality
secondary_mortality <- cms_outcomes %>%
  filter(measure_id %in% c("MORT_30_HF", "MORT_30_PN")) %>%
  filter(!is.na(score)) %>%
  inner_join(onc_clean, by = "facility_id") %>%
  mutate(
    log_denominator = log(denominator),
    tefca_status = factor(
      tefca_status,
      levels = c(
        "Planned TEFCA",
        "Current TEFCA",
        "Neither current nor planned"
      )
    )
  )

write_csv(primary_hybrid_hwm, here("data_processed", "primary_hybrid_hwm.csv"))
write_csv(secondary_mortality, here("data_processed", "secondary_mortality.csv"))

# Merge diagnostics
merge_summary <- tibble(
  step = c(
    "ONC rows after excluding missing facility IDs",
    "ONC unique hospitals",
    "CMS Hybrid HWM rows with nonmissing score",
    "Primary merged Hybrid HWM analytic rows",
    "Secondary merged mortality analytic rows"
  ),
  n = c(
    nrow(onc_clean),
    n_distinct(onc_clean$facility_id),
    cms_outcomes %>% filter(measure_id == "Hybrid_HWM", !is.na(score)) %>% nrow(),
    nrow(primary_hybrid_hwm),
    nrow(secondary_mortality)
  )
)

write_csv(merge_summary, here("outputs", "tables", "merge_summary.csv"))

cat("\nMerge summary:\n")
print(merge_summary)

cat("\nPrimary analytic sample by TEFCA status:\n")
print(table(primary_hybrid_hwm$tefca_status, useNA = "ifany"))

cat("\nSecondary analytic sample by measure and TEFCA status:\n")
print(table(secondary_mortality$measure_id, secondary_mortality$tefca_status, useNA = "ifany"))