# Function to add sine and cosine terms to a weekly timeseries for fitting seasonal terms in a glm
# input ts a tibble with a weekly timeseries
create_season_data <- function(ts) {
  ts_len <- nrow(ts)
  sin_cos <- data.frame(
    sin = sin(2 * pi * (1:ts_len) / 52),
    cos = cos(2 * pi * (1:ts_len) / 52)
  )
  ts %>% bind_cols(sin_cos)
}

# Function to plot a timeseries and optionally the results of a signal detection algorithm such as expectation, threshold and signals if these were computed
# This function assumes that the last week is the week for which outbreak detection was performed and colors the bar differently
# input parameter ts tibble with weekly timeseries data consisting of a variable date and cases, optionally a column with threshold, expectation and alarms can be provided
plot_ts <- function(ts, with_signals = FALSE) {
  plot <- ggplot(ts, aes(x = date, y = cases)) +
    geom_bar(stat = "identity") +
    theme_bw()

  if(any(c("expectation", "threshold") %in% names(ts))){
    ts_for_lines <- ts
    max_date <- max(ts$date)
    # mark the last datapoint in the ts as Signal Detection Week
    ts <- ts %>%
      mutate(category = if_else(date == max_date, "Signal Detection week", "Training Data"))

    plot <- ggplot() +
      geom_bar(data = ts, aes(x = date, y = cases, fill = category), stat = "identity")+
      scale_fill_manual(values = c("Signal Detection week" = "orange", "Training Data" = "#999"))
  }

  color_values <- c()

  if("expectation" %in% names(ts)){
    plot <- plot +
      geom_line(data = ts_for_lines, aes(x = date, y = expectation, color = "Expectation"), linewidth = 1)
    color_values["Expectation"] = "black"

  }
  if ("threshold" %in% names(ts)){
    plot <- plot +
      geom_line(data = ts_for_lines, aes(x = date, y = threshold, color = "Threshold"), linewidth = 1)
    color_values["Threshold"]   = "blue"

  }
  if (any(ts$alarms == 1, na.rm = TRUE)){
    plot <- plot +
      geom_point(data = filter(ts, alarms == 1), aes(x = date, y = cases, color = "Signals"), shape = 4, size = 2, stroke = 2)
    color_values["Signals"]     = "red"
  }

  plot <- plot +
  labs(color = "") +
  scale_color_manual(values = color_values) +
  labs(color = NULL, fill = NULL) + # Removes titles for both color and fill legends
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(override.aes = list(shape = NA)) # Order bar legend second
      )

  plot
}
