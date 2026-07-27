primary_hybrid_hwm %>%
  summarise(
    min_score = min(score, na.rm = TRUE),
    max_score = max(score, na.rm = TRUE),
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE)
  )