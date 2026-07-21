# 07_primary_coefficient_plot.R
# Purpose: Create coefficient plot for the primary Hybrid HWM model.

library(tidyverse)
library(here)
library(broom)

# Load fitted primary model from script 05
primary_model <- readRDS(
  here("outputs", "models", "primary_hybrid_hwm_model.rds")
)

# Exact display labels used in both recode() and factor levels.
display_levels <- rev(c(
  "Current vs planned",
  "Neither vs planned",
  "National network",
  "EHR vendor network",
  "State/regional/local HIO",
  "Log CMS denominator"
))

# Extract main coefficients
coef_plot_data <- broom::tidy(
  primary_model,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
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
      "tefca_statusCurrent TEFCA" = "Current vs planned",
      "tefca_statusNeither current nor planned" = "Neither vs planned",
      "national_network" = "National network",
      "vendor_network" = "EHR vendor network",
      "hio" = "State/regional/local HIO",
      "log_denominator" = "Log CMS denominator"
    ),
    predictor = factor(
      predictor,
      levels = display_levels
    )
  )

# Check that no display labels became missing.
if (any(is.na(coef_plot_data$predictor))) {
  stop(
    "At least one coefficient label is missing. ",
    "Check that recode labels exactly match display_levels."
  )
}

# Save coefficient data
write_csv(
  coef_plot_data,
  here(
    "outputs",
    "tables",
    "figureA3_primary_coefficient_plot_data.csv"
  )
)

# Create coefficient plot
figureA3 <- ggplot(
  coef_plot_data,
  aes(
    x = predictor,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
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

# Save as PNG and PDF
ggsave(
  here(
    "outputs",
    "figures",
    "figureA3_primary_coefficient_plot.png"
  ),
  figureA3,
  width = 7.5,
  height = 4.5,
  dpi = 300
)

ggsave(
  here(
    "outputs",
    "figures",
    "figureA3_primary_coefficient_plot.pdf"
  ),
  figureA3,
  width = 7.5,
  height = 4.5
)

cat("\nFigure A3 coefficient plot data:\n")
print(coef_plot_data)
cat("\nSaved Figure A3 to outputs/figures.\n")
