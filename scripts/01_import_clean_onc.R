# 01_import_clean_onc.R
# Purpose: Import and clean ONC hospital TEFCA/network participation data.

library(tidyverse)
library(janitor)
library(here)

onc_raw <- read_csv(
  here("data_raw", "hospital-TEFCA-Network-participation.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

onc_clean <- onc_raw %>%
  mutate(
    facility_id = str_pad(as.character(as.integer(mcrnum)), width = 6, side = "left", pad = "0"),
    year = as.integer(year),
    national_network = as.integer(national_network),
    vendor_network = as.integer(vendor_network),
    hio = as.integer(hio),
    tefca_part = as.integer(tefca_part),
    tefca_plan = as.integer(tefca_plan),
    
    tefca_status = case_when(
      tefca_part == 1 ~ "Current TEFCA",
      tefca_part != 1 & tefca_plan == 1 ~ "Planned TEFCA",
      tefca_part != 1 & tefca_plan != 1 ~ "Neither current nor planned",
      TRUE ~ NA_character_
    ),
    
    tefca_status = factor(
      tefca_status,
      levels = c(
        "Planned TEFCA",
        "Current TEFCA",
        "Neither current nor planned"
      )
    )
  ) %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(!is.na(facility_id)) %>%
  select(
    aha_id,
    mstate,
    facility_id,
    year,
    national_network,
    vendor_network,
    hio,
    tefca_part,
    tefca_plan,
    tefca_status
  )

write_csv(onc_clean, here("data_processed", "onc_clean.csv"))

cat("ONC rows:", nrow(onc_clean), "\n")
cat("Unique hospitals:", n_distinct(onc_clean$facility_id), "\n")
print(table(onc_clean$year, useNA = "ifany"))
print(table(onc_clean$tefca_status, useNA = "ifany"))