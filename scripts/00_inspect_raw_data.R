# 00_inspect_raw_data.R
# Purpose: Inspect raw ONC and CMS files before cleaning.

library(tidyverse)
library(janitor)
library(here)
library(skimr)

# List raw files
raw_files <- list.files(here("data_raw"), full.names = TRUE)
print(raw_files)

# Read all CSV files just enough to inspect structure
raw_preview <- map(raw_files, ~ read_csv(.x, n_max = 10, show_col_types = FALSE))

names(raw_preview) <- basename(raw_files)

# Print column names for each file
for (file_name in names(raw_preview)) {
  cat("\n\n=============================\n")
  cat("FILE:", file_name, "\n")
  cat("=============================\n")
  print(names(raw_preview[[file_name]]))
  cat("\nFirst rows:\n")
  print(head(raw_preview[[file_name]], 3))
}

# Save column names to a text file for reference
dir.create(here("outputs"), showWarnings = FALSE)
dir.create(here("outputs/tables"), showWarnings = FALSE)

sink(here("outputs/tables/raw_file_column_names.txt"))
for (file_name in names(raw_preview)) {
  cat("\n\n=============================\n")
  cat("FILE:", file_name, "\n")
  cat("=============================\n")
  print(names(raw_preview[[file_name]]))
}
sink()