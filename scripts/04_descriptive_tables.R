# 04_descriptive_tables.R
# Purpose: Create cohort construction and descriptive tables by TEFCA status.

library(tidyverse)
library(here)
library(writexl)
library(gt)

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
    )
  )

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
    )
  )

merge_summary <- read_csv(
  here("outputs", "tables", "merge_summary.csv"),
  show_col_types = FALSE
)

# Helper functions
fmt_n_pct <- function(x) {
  x <- x[!is.na(x)]
  n <- sum(x)
  d <- length(x)
  if (d == 0) return(NA_character_)
  sprintf("%d (%.1f%%)", n, 100 * n / d)
}

fmt_mean_sd <- function(x, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

fmt_median_iqr <- function(x, digits = 1) {
  q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
    q[[2]], q[[1]], q[[3]]
  )
}

# Descriptive characteristics by TEFCA status
table1_grouped <- primary_hybrid_hwm %>%
  group_by(tefca_status) %>%
  summarise(
    `Hospitals, n` = as.character(n()),
    `ONC year 2023, n (%)` = fmt_n_pct(year == 2023),
    `ONC year 2024, n (%)` = fmt_n_pct(year == 2024),
    `States represented, n` = as.character(n_distinct(mstate)),
    `National network participation, n (%)` = fmt_n_pct(national_network == 1),
    `EHR vendor-network participation, n (%)` = fmt_n_pct(vendor_network == 1),
    `State/regional/local HIO participation, n (%)` = fmt_n_pct(hio == 1),
    `CMS denominator, median [IQR]` = fmt_median_iqr(denominator, digits = 0),
    `CMS denominator, mean (SD)` = fmt_mean_sd(denominator, digits = 1),
    `Hybrid HWM score, median [IQR]` = fmt_median_iqr(score, digits = 2),
    `Hybrid HWM score, mean (SD)` = fmt_mean_sd(score, digits = 2),
    .groups = "drop"
  )

table1_wide <- table1_grouped %>%
  pivot_longer(
    cols = -tefca_status,
    names_to = "Characteristic",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = tefca_status,
    values_from = Value
  ) %>%
  select(
    Characteristic,
    `Planned TEFCA`,
    `Current TEFCA`,
    `Neither current nor planned`
  )

# Secondary sample counts by outcome and TEFCA status
secondary_sample_counts <- secondary_mortality %>%
  count(measure_id, measure_name, tefca_status) %>%
  pivot_wider(
    names_from = tefca_status,
    values_from = n,
    values_fill = 0
  ) %>%
  select(
    measure_id,
    measure_name,
    `Planned TEFCA`,
    `Current TEFCA`,
    `Neither current nor planned`
  )

# Save CSV files
write_csv(
  table1_wide,
  here("outputs", "tables", "table1_descriptives_by_tefca.csv")
)

write_csv(
  secondary_sample_counts,
  here("outputs", "tables", "secondary_sample_counts.csv")
)

# Save Excel workbook for easier copying into Google Docs
write_xlsx(
  list(
    cohort_construction = merge_summary,
    descriptives_by_tefca = table1_wide,
    secondary_sample_counts = secondary_sample_counts
  ),
  here("outputs", "tables", "table1_and_sample_counts.xlsx")
)

# Save HTML version of Table 1
table1_gt <- table1_wide %>%
  gt() %>%
  tab_header(
    title = "Table 1",
    subtitle = "Descriptive characteristics by TEFCA participation status"
  ) %>%
  cols_label(
    Characteristic = "Characteristic"
  )

gtsave(
  table1_gt,
  here("outputs", "tables", "table1_descriptives_by_tefca.html")
)

cat("\nTable 1 preview:\n")
print(table1_wide, n = Inf)

cat("\nSecondary sample counts:\n")
print(secondary_sample_counts)