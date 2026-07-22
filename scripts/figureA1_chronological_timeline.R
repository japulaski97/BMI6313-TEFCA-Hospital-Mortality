# figureA1_chronological_timeline.R
# Purpose: Create a chronological timeline showing TEFCA milestones
# alongside the ONC survey years and CMS mortality measurement periods.

library(tidyverse)
library(here)

# Create output directories if they do not already exist.
dir.create(
  here("outputs", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Survey and CMS mortality measurement periods.
# PSI complications were removed because they were not analyzed in this study.
windows <- tribble(
  ~row, ~label, ~start, ~end,
  
  3, "Condition-specific mortality",
  as.Date("2021-07-01"), as.Date("2024-06-30"),
  
  2, "ONC survey years",
  as.Date("2023-01-01"), as.Date("2024-12-31"),
  
  1, "Hybrid HWM",
  as.Date("2023-07-01"), as.Date("2024-06-30")
)

# Point-in-time TEFCA milestones.
#
# Month-level events are plotted on the first day of the stated month.
# Each milestone includes:
#   date          = plotted milestone date
#   line_end_date = end of the short leader line
#   line_end_y    = vertical end of the leader line
#   text_date     = horizontal position of the annotation
#   text_y        = vertical position of the annotation
#   hjust         = horizontal text alignment
milestones <- tribble(
  ~date, ~label, ~line_end_date, ~line_end_y,
  ~text_date, ~text_y, ~hjust,
  
  as.Date("2016-12-01"),
  "21st Century Cures Act\n(December 2016)",
  as.Date("2017-01-10"), 4.20,
  as.Date("2017-01-22"), 4.28, 0,
  
  as.Date("2022-01-01"),
  "TEFCA Version 1 published\n(January 2022)",
  as.Date("2021-11-20"), 4.20,
  as.Date("2021-11-05"), 4.28, 1,
  
  as.Date("2023-12-01"),
  "TEFCA go-live and initial\nQHIN designations\n(December 2023)",
  as.Date("2023-10-05"), 4.28,
  as.Date("2023-09-20"), 4.38, 1,
  
  # Place this annotation below and to the left of its point so that it
  # remains fully inside the exported figure.
  as.Date("2024-02-01"),
  "Additional QHIN designations\n(February 2024)",
  as.Date("2023-12-20"), 3.78,
  as.Date("2023-12-05"), 3.68, 1
)

figureA1 <- ggplot() +
  
  # ONC survey and CMS mortality periods
  geom_segment(
    data = windows,
    aes(
      x = start,
      xend = end,
      y = row,
      yend = row
    ),
    linewidth = 4,
    lineend = "butt"
  ) +
  
  geom_point(
    data = windows,
    aes(x = start, y = row),
    size = 1.8
  ) +
  
  geom_point(
    data = windows,
    aes(x = end, y = row),
    size = 1.8
  ) +
  
  # TEFCA milestone lane
  geom_hline(
    yintercept = 4,
    linewidth = 0.45
  ) +
  
  geom_point(
    data = milestones,
    aes(x = date, y = 4),
    size = 2.5
  ) +
  
  # Short leader lines ending before the text
  geom_segment(
    data = milestones,
    aes(
      x = date,
      xend = line_end_date,
      y = 4,
      yend = line_end_y
    ),
    linewidth = 0.35
  ) +
  
  # Milestone labels
  geom_text(
    data = milestones,
    aes(
      x = text_date,
      y = text_y,
      label = label,
      hjust = hjust
    ),
    size = 2.9,
    lineheight = 0.93
  ) +
  
  scale_x_date(
    limits = as.Date(c("2016-06-01", "2025-02-01")),
    breaks = as.Date(paste0(2017:2025, "-01-01")),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "Hybrid HWM",
      "ONC survey years",
      "Condition-specific mortality",
      "TEFCA milestones"
    ),
    limits = c(0.55, 4.72)
  ) +
  
  labs(
    x = "Calendar year",
    y = NULL
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 10.5) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(hjust = 1),
    plot.margin = margin(
      t = 20,
      r = 35,
      b = 12,
      l = 14
    )
  )

# Save PNG for insertion into the manuscript.
ggsave(
  filename = here(
    "outputs",
    "figures",
    "figureA1_chronological_timeline.png"
  ),
  plot = figureA1,
  width = 10.8,
  height = 5.2,
  dpi = 300
)

# Save PDF as the vector-quality version.
ggsave(
  filename = here(
    "outputs",
    "figures",
    "figureA1_chronological_timeline.pdf"
  ),
  plot = figureA1,
  width = 10.8,
  height = 5.2
)

# Preserve the figure's underlying source data.
write_csv(
  windows,
  here(
    "outputs",
    "tables",
    "figureA1_measurement_windows.csv"
  )
)

write_csv(
  milestones,
  here(
    "outputs",
    "tables",
    "figureA1_tefca_milestones.csv"
  )
)

cat(
  "\nSaved the revised Figure A1 to outputs/figures.\n"
)