create_season_data <- function(ts) {
  ts_len <- nrow(ts)
  sin_cos <- data.frame(
    sin = sin(2 * pi * (1:ts_len) / 52),
    cos = cos(2 * pi * (1:ts_len) / 52)
  )
  ts %>% bind_cols(sin_cos)
}

plot_ts <- function(ts, with_signals = FALSE) {
  plot <- ggplot(ts, aes(x = date, y = cases)) +
    geom_bar(stat = "identity") +
    theme_bw()

  if (with_signals) {
    # Separate data for bars and lines
    ts_for_lines <- ts

    ts <- ts %>%
      mutate(category = if_else(!is.na(alarms), "Signal Detection weeks", "Training Data"))

    plot <- ggplot() +
      geom_bar(data = ts, aes(x = date, y = cases, fill = category), stat = "identity") +
      geom_line(data = ts_for_lines, aes(x = date, y = threshold, color = "Threshold"), linewidth = 1) +
      geom_line(data = ts_for_lines, aes(x = date, y = expectation, color = "Expectation"), linewidth = 1) +
      {
        if (any(ts$alarms == 1, na.rm = TRUE))
          geom_point(data = filter(ts, alarms == 1), aes(x = date, y = cases, color = "Signals"), shape = 4, size = 2, stroke = 2)
      } +
      labs(color = "") +
      scale_color_manual(values = c("Threshold" = "blue", "Expectation" = "black", "Signals" = "red")) +
      scale_fill_manual(values = c("Signal Detection weeks" = "orange", "Training Data" = "#999")) +
      labs(color = NULL, fill = NULL) + # Removes titles for both color and fill legends
      theme_bw() +
      theme(legend.position = "bottom") +
      guides(
        fill = guide_legend(override.aes = list(shape = NA)) # Order bar legend second
      )
  }
  plot
}



complete_weeks_aggr_data <- function(ts_aggregated) {
  min_isoyear <- min(ts_aggregated$year)
  max_isoyear <- max(ts_aggregated$year)
  last_week <- max(ts_aggregated %>%
    filter(year == max_isoyear) %>%
    pull(week))

  years <- seq(min_isoyear, max_isoyear, 1)
  week_year_grid <- bind_rows(lapply(years, function(year) {
    # Check if the last day of the year is in week 53
    last_day <- as.Date(paste0(year, "-12-31"))
    max_week <- ifelse(isoweek(last_day) == 53, 53, 52)

    # Generate all weeks for the year
    expand_grid(year = year, week = 1:max_week)
  }))
  # remove all weeks in after the last week in the original aggregated dataset
  week_year_grid <- week_year_grid %>%
    filter(!(year == max_isoyear & week > last_week))

  week_year_grid %>%
    left_join(ts_aggregated, by = c("week", "year")) %>%
    replace_na(list(cases = 0))
}
