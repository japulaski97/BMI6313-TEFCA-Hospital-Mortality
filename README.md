# Early TEFCA Participation and Hospital Mortality

This repository contains the data-processing and analysis workflow for an unpublished BMI 6313 scientific-writing project examining whether early participation in the Trusted Exchange Framework and Common Agreement (TEFCA) was associated with hospital-level mortality outcomes.

## Headline finding

Among 691 U.S. non-federal acute care hospitals, the analyses did not provide robust evidence that current TEFCA participation was associated with lower mortality than planned participation. In the primary model, current participation was associated with a 0.108-point higher CMS Hybrid Hospital-Wide 30-Day Mortality (Hybrid HWM) score than planned participation (95% CI, 0.001–0.216; *p* = .047), but the confidence interval included zero when HC3 robust standard errors were used (95% CI, −0.010–0.227; *p* = .072). Current-versus-planned estimates were also not statistically significant for heart failure or pneumonia mortality.

These results describe between-hospital associations during early TEFCA implementation and should not be interpreted as causal effects.

## Poster materials

- **Scientific poster:** in development
- **Unpublished class paper:** to be added
- **Analysis code:** available in [`scripts/`](scripts/)
- **Tables and figures:** available in [`outputs/`](outputs/)

The final poster PDF and paper will be linked here before this repository is used as the poster's QR-code destination.

## Study overview

- **Design:** cross-sectional, hospital-level observational analysis
- **Cohort:** 691 U.S. non-federal acute care hospitals
- **Exposure:** current, planned, or neither current nor planned TEFCA participation
- **Headline comparison:** current versus planned participation
- **Primary outcome:** CMS Hybrid HWM 30-day mortality score
- **Secondary outcomes:** CMS heart failure and pneumonia 30-day mortality measures
- **Model:** ordinary least squares regression adjusted for national-network, EHR vendor-network, and state/regional/local HIO participation; log CMS denominator; state fixed effects; and ONC survey-year fixed effects

## Data sources

This analysis uses public, hospital-level data from:

- [ONC: U.S. Hospital Participation in Health Information Networks](https://healthit.gov/data/datasets/hospital-network-participation/)
- [CMS: Complications & Deaths — Hospitals](https://data.cms.gov/provider-data/topics/hospitals/complications-deaths)

The analysis files in this repository reflect the ONC 2023–2024 survey data and the CMS mortality-measure files used for the study. Source webpages may subsequently display newer releases.

## Repository structure

- `data_raw/`: original downloaded datasets
- `data_processed/`: cleaned and merged analytic datasets
- `scripts/`: R scripts for inspection, cleaning, linkage, modeling, and sensitivity analyses
- `outputs/tables/`: exported descriptive and model tables
- `outputs/figures/`: exported figures and diagrams
- `BMI6313-TEFCA-Hospital-Mortality.Rproj`: RStudio project file
- `renv.lock`: recorded R package environment

## Reproducibility

This project uses R and RStudio with `renv` for package management.

1. Clone or download this repository.
2. Open `BMI6313-TEFCA-Hospital-Mortality.Rproj` in RStudio.
3. Run `renv::restore()` to recreate the recorded package environment, if needed.
4. Confirm that the required ONC and CMS source files are present in `data_raw/`.
5. Run the analysis scripts in numerical order, from `scripts/00_inspect_raw_data.R` through `scripts/08_sensitivity_analyses.R`.
6. Review exported tables in `outputs/tables/` and figures in `outputs/figures/`.

## Contact

**Jordan Pulaski**  
McWilliams School of Biomedical Informatics at UTHealth Houston  
[GitHub profile](https://github.com/japulaski97)
