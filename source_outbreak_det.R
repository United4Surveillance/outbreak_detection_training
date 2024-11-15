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


# Add missing isoweeks to an aggregated dataframe of case counts by year and week
#
#  This function takes a data frame containing year-week and case count and ensures
#  that it includes all possible year-week combinations within the specified
#  range. It fills in missing rows with 0 values for cases and returns the
#  updated data frame.
add_missing_isoweeks <- function(ts_aggregated) {
  
  ts_aggregated <- ts_aggregated %>%
    mutate(date = isoweek_to_date(week, year))
  
  min_date <- min(ts_aggregated$date)
  max_date <- max(ts_aggregated$date)
  # Generate a sequence of dates from start to end date
  # we add the date_end because the seq ends before date_end when there is a partial week remaining and we also want to have the isoweek of the date_end
  date_seq <- c(seq.Date(from = as.Date(min_date), to = as.Date(max_date), by = "week"), max_date)
  # Create a data frame with ISO weeks and ISO years
  df_all_years_weeks <- data.frame(
    year = isoyear(date_seq),
    week = isoweek(date_seq)
  ) %>%
    # in case date_end is there twice due to adding before
    distinct(year, week)
  
  # Merge the template with the original data to fill in missing rows
  data_agg_complete <- merge(ts_aggregated, df_all_years_weeks, by = c("year", "week"), all = TRUE) %>%
    dplyr::select(-date) %>%
    replace_na(list(cases = 0)) %>% 
    arrange(year, week)
}

isoweek_to_date <- function(week, year) {
  # Format the ISO week string
  iso_week_str <- sprintf("%d-W%02d-1", year, week)
  
  # Convert to date (the first day of the ISO week)
  date <- ISOweek::ISOweek2date(iso_week_str)
  return(date)
}
