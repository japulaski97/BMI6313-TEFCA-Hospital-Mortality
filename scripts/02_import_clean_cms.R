# 02_import_clean_cms.R
# Purpose: Import and inspect CMS Complications and Deaths - Hospital data.

library(tidyverse)
library(janitor)
library(here)

cms_raw <- read_csv(
  here("data_raw", "Complications_and_Deaths-Hospital.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

cms_clean <- cms_raw %>%
  mutate(
    facility_id = as.character(facility_id),
    
    denominator = parse_number(
      as.character(denominator),
      na = c("", "NA", "N/A", "Not Available", "Not Applicable")
    ),
    
    score = parse_number(
      as.character(score),
      na = c("", "NA", "N/A", "Not Available", "Not Applicable")
    ),
    
    lower_estimate = parse_number(
      as.character(lower_estimate),
      na = c("", "NA", "N/A", "Not Available", "Not Applicable")
    ),
    
    higher_estimate = parse_number(
      as.character(higher_estimate),
      na = c("", "NA", "N/A", "Not Available", "Not Applicable")
    )
  )

cms_measure_inventory <- cms_clean %>%
  count(measure_id, measure_name, start_date, end_date, sort = TRUE)

write_csv(cms_clean, here("data_processed", "cms_clean_all_measures.csv"))
write_csv(cms_measure_inventory, here("outputs", "tables", "cms_measure_inventory.csv"))

cms_measure_search <- cms_measure_inventory %>%
  filter(
    str_detect(
      str_to_lower(paste(measure_id, measure_name)),
      "hybrid|hwm|heart|failure|pneumonia|pn|mort"
    )
  )

write_csv(cms_measure_search, here("outputs", "tables", "cms_measure_search.csv"))

cat("CMS rows:", nrow(cms_clean), "\n")
cat("Unique facilities:", n_distinct(cms_clean$facility_id), "\n")
cat("Unique measures:", n_distinct(cms_clean$measure_id), "\n")

print(cms_measure_search, n = 50)