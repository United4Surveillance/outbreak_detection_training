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
