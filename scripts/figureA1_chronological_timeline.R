# figureA1_chronological_timeline_clean_v4.R
# Purpose: Create a clean chronological timeline with short leader lines
# that stop before the milestone labels.

library(tidyverse)
library(here)

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

# Survey and measurement windows
windows <- tribble(
  ~row, ~label, ~start, ~end,
  4, "Condition-specific mortality",
  as.Date("2021-07-01"), as.Date("2024-06-30"),
  3, "PSI complications",
  as.Date("2022-07-01"), as.Date("2024-06-30"),
  2, "ONC survey years",
  as.Date("2023-01-01"), as.Date("2024-12-31"),
  1, "Hybrid HWM",
  as.Date("2023-07-01"), as.Date("2024-06-30")
)

# Each milestone has:
#   - the true event date;
#   - a short leader-line endpoint;
#   - a nearby text position beyond the leader endpoint.
milestones <- tribble(
  ~date, ~label, ~line_end_date, ~line_end_y, ~text_date, ~text_y, ~hjust,
  as.Date("2016-12-01"),
  "21st Century Cures Act\n(December 2016)",
  as.Date("2017-01-10"), 5.24,
  as.Date("2017-01-22"), 5.32, 0,

  as.Date("2022-01-01"),
  "TEFCA Version 1 published\n(January 2022)",
  as.Date("2021-11-20"), 5.24,
  as.Date("2021-11-05"), 5.32, 1,

  as.Date("2023-12-01"),
  "TEFCA go-live and initial\nQHIN designations\n(December 2023)",
  as.Date("2023-09-25"), 5.34,
  as.Date("2023-09-10"), 5.43, 1,

  as.Date("2024-02-01"),
  "Additional QHIN designations\n(February 2024)",
  as.Date("2024-04-15"), 4.76,
  as.Date("2024-04-28"), 4.67, 0
)

figureA1 <- ggplot() +
  # Measurement/survey spans
  geom_segment(
    data = windows,
    aes(
      x = start,
      xend = end,
      y = row,
      yend = row
    ),
    linewidth = 3.8,
    lineend = "butt"
  ) +
  geom_point(
    data = windows,
    aes(x = start, y = row),
    size = 1.7
  ) +
  geom_point(
    data = windows,
    aes(x = end, y = row),
    size = 1.7
  ) +

  # Milestone lane and points
  geom_hline(
    yintercept = 5,
    linewidth = 0.45
  ) +
  geom_point(
    data = milestones,
    aes(x = date, y = 5),
    size = 2.5
  ) +

  # Short leader lines that terminate before the text
  geom_segment(
    data = milestones,
    aes(
      x = date,
      xend = line_end_date,
      y = 5,
      yend = line_end_y
    ),
    linewidth = 0.35
  ) +

  # Labels positioned just beyond each leader-line endpoint
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
    breaks = 1:5,
    labels = c(
      "Hybrid HWM",
      "ONC survey years",
      "PSI complications",
      "Condition-specific mortality",
      "TEFCA milestones"
    ),
    limits = c(0.55, 5.72)
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
    plot.margin = margin(18, 22, 10, 12)
  )

ggsave(
  here(
    "outputs",
    "figures",
    "figureA1_chronological_timeline.png"
  ),
  figureA1,
  width = 10.8,
  height = 5.8,
  dpi = 300
)

ggsave(
  here(
    "outputs",
    "figures",
    "figureA1_chronological_timeline.pdf"
  ),
  figureA1,
  width = 10.8,
  height = 5.8
)

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
  "\nSaved the revised Figure A1 with short leader lines to outputs/figures.\n"
)
