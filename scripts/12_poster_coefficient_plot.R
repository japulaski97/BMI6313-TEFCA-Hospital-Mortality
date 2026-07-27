# 12_poster_coefficient_plot.R
# Purpose: Create a poster-formatted coefficient plot for the
# primary Hybrid HWM model.
#
# This script changes presentation only. It uses coefficient
# estimates previously saved by 07_primary_coefficient_plot.R.

library(tidyverse)
library(here)

# Display order from top to bottom after coord_flip().
display_levels <- rev(c(
  "Current vs planned",
  "Neither vs planned",
  "National network",
  "EHR vendor network",
  "State/regional/local HIO",
  "Log CMS denominator"
))

# Load the coefficient data created for the original paper figure.
coef_plot_data <- read_csv(
  here(
    "outputs",
    "tables",
    "figureA3_primary_coefficient_plot_data.csv"
  ),
  show_col_types = FALSE
)

# Verify that all expected coefficients are present.
missing_labels <- setdiff(
  display_levels,
  coef_plot_data$predictor
)

if (length(missing_labels) > 0) {
  stop(
    "Missing coefficient labels: ",
    paste(missing_labels, collapse = ", ")
  )
}

coef_plot_data <- coef_plot_data %>%
  mutate(
    predictor = factor(
      predictor,
      levels = display_levels
    )
  )

# Create poster-formatted coefficient plot.
poster_coefficient_plot <- ggplot(
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
    linetype = "dashed",
    linewidth = 0.9,
    color = "gray40"
  ) +
  geom_errorbar(
    width = 0.16,
    linewidth = 1.15,
    color = "#0E5F70"
  ) +
  geom_point(
    size = 4.2,
    shape = 21,
    stroke = 0.9,
    color = "#0E4F5B",
    fill = "#2B9BAD"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0.07, 0.07))
  ) +
  labs(
    x = NULL,
    y = "Adjusted coefficient estimate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_text(
      size = 15,
      color = "black"
    ),
    axis.text.x = element_text(
      size = 13,
      color = "black"
    ),
    axis.title.x = element_text(
      size = 15,
      margin = margin(t = 10)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "gray85",
      linewidth = 0.5
    ),
    plot.margin = margin(10, 12, 8, 8)
  )

# Save poster-specific outputs without overwriting the paper figure.
ggsave(
  here(
    "outputs",
    "figures",
    "poster_primary_coefficient_plot.png"
  ),
  poster_coefficient_plot,
  width = 10.5,
  height = 4.5,
  dpi = 600,
  bg = "white"
)

ggsave(
  here(
    "outputs",
    "figures",
    "poster_primary_coefficient_plot.pdf"
  ),
  poster_coefficient_plot,
  width = 10.5,
  height = 4.5
)

print(poster_coefficient_plot)

cat(
  "\nSaved poster coefficient plot to outputs/figures ",
  "as PNG and PDF.\n",
  sep = ""
)