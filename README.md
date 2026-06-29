# BMI6313 TEFCA Hospital Mortality Analysis

This project analyzes whether early TEFCA participation is associated with hospital-level mortality outcomes using ONC hospital network-participation data and CMS Complications and Deaths - Hospital data.

## Folder structure

- `data_raw/`: original downloaded datasets
- `data_processed/`: cleaned and merged analytic datasets
- `scripts/`: R scripts for cleaning, merging, and analysis
- `outputs/tables/`: exported model and descriptive tables
- `outputs/figures/`: exported figures and diagrams
- `manuscript/`: notes and manuscript-related materials

## Reproducibility

This project uses R and RStudio with `renv` for package management. To reproduce the analysis:

1. Open `BMI6313-TEFCA-Hospital-Mortality.Rproj` in RStudio.
2. Run `renv::restore()` if recreating the package environment.
3. Confirm that the raw ONC and CMS CSV files are in `data_raw/`.
4. Run scripts in order from `scripts/00_inspect_raw_data.R` through `scripts/08_sensitivity_analyses.R`.
5. Tables are saved in `outputs/tables/`; figures are saved in `outputs/figures/`.

Primary analysis:
- Outcome: CMS Hybrid HWM score
- Exposure: TEFCA participation status
- Reference group: planned-but-not-current TEFCA participation
- Model: OLS linear regression with state and ONC survey-year fixed effects