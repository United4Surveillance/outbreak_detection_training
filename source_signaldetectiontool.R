app_cache_env <- new.env()
app_cache_env <- new.env()
app_cache_env$sex_levels <- c("male", "female", "diverse", NA_character_)
app_cache_env$age_group_levels <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100-104", "105-109", NA_character_)


#' Preprocessing of linelist surveillance data with or without outbreak_ids
#' @param data data.frame, Linelist of surveillance data
#' @returns data.frame, preprocessed linelist with transformation of columns to date,
#' to lower, generation of isoyear and isoweek
#'
#' @export
#'
#' @examples
#' \dontrun{
#' preprocess_data(input_example)
#' }
preprocess_data <- function(data) {
  # remove completely empty columns from the dataset
  data <- remove_empty_columns(data)

  yes_no_unknown_vars <- intersect(colnames(data), yes_no_unknown_variables())
  # get all variables present in the data which might need transformation tolower
  to_lower_vars <- intersect(colnames(data), c(yes_no_unknown_variables(), "sex"))
  # get all regional stratification variables
  regional_id_vars <- intersect(colnames(data), region_id_variable_names())

  # get all variables that are characters and not case_id or date
  factorization_vars <- dplyr::select(data, dplyr::where(is.character) &
                                        !dplyr::any_of(c("sex", "age_group")) &
                                        !dplyr::all_of(yes_no_unknown_vars) &
                                        !dplyr::starts_with("date") &
                                        !dplyr::ends_with("id")) %>% names()

  # remove cases with missing values
  data <- data %>%
    dplyr::filter_at(check_for_missing_values(), dplyr::all_vars(!is.na(.)))

  data <- data %>%
    # strip trailing or leading whitespaces
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_lower_vars), ~ tolower(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "unknown"))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "NA"))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date"), ~ as.Date(.x, optional = T))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(regional_id_vars), ~ as.character(.x))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(yes_no_unknown_vars),
      ~ factor(.x, levels = unlist(yes_no_unknown_levels()))
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factorization_vars), ~ as.factor(.x)))


  # add columns for isoyear and isoweek for each date
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
                                ~ surveillance::isoWeekYear(.x)$ISOYear,
                                .names = "{.col}_year"
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("date") & !dplyr::where(is.numeric),
                                ~ surveillance::isoWeekYear(.x)$ISOWeek,
                                .names = "{.col}_week"
    ))

  if("age" %in% names(data())){
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of("age"), ~ dplyr::if_else(.x < 0, NA_integer_, .x)))
  }
  # age or age_group is mandatory thus we need to check whether column present in data
  # or else create age_group from age
  data <- age_groups(data)

  # sex is not mandatory
  if ("sex" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(sex = factor(sex, levels = sex_levels()))
  }

  data
}

#' Aggregates case data (linelist, i.e. one row per case) by isoyear and isoweek and adds missing isoweeks to the aggregated dataset.
#' Additionally number of cases part of a known outbreak is added if the variable outbreak_status exists in the data.
#'
#' @param data data.frame, linelist of cases to be aggregated
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param date_start A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the minimum date of the dataset. This parameter can be used when the dataset should be extended further than the minimum date of the dataset.
#' @param date_end A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the maximum date of the dataset. This can be used when the dataset should be extended further than the minimum date of the dataset.
#' @examples
#' \dontrun{
#' data <- preprocess_data(input_example) %>% aggregate_data()
#' }
#' @export
aggregate_data <- function(data,
                           date_var = "date_report",
                           date_start = NULL,
                           date_end = NULL) {
  checkmate::check_subset(date_var, names(data))

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  week_var <- paste0(date_var, "_week")
  year_var <- paste0(date_var, "_year")

  data_agg <- data %>%
    dplyr::group_by(!!rlang::sym(week_var), !!rlang::sym(year_var)) %>%
    dplyr::summarize(cases = dplyr::n(), .groups = "drop") %>%
    dplyr::select(
      week = !!rlang::sym(week_var),
      year = !!rlang::sym(year_var),
      cases
    ) %>%
    dplyr::arrange(year, week)

  # add the missing isoweeks to the dataset
  data_agg <- data_agg %>% add_missing_isoweeks(
    date_start = date_start,
    date_end = date_end
  )

  if ("outbreak_status" %in% names(data)) {
    data_outbreak_agg <- data %>%
      dplyr::group_by(!!rlang::sym(week_var), !!rlang::sym(year_var)) %>%
      dplyr::summarize(
        cases_in_outbreak = sum(outbreak_status == "yes", na.rm = T),
        .groups = "drop"
      ) %>%
      dplyr::select(
        week = !!rlang::sym(week_var),
        year = !!rlang::sym(year_var),
        cases_in_outbreak
      ) %>%
      dplyr::arrange(year, week)

    data_agg <- data_agg %>%
      dplyr::left_join(data_outbreak_agg, by = c("year", "week")) %>%
      dplyr::mutate(cases_in_outbreak = dplyr::if_else(is.na(cases_in_outbreak), 0, cases_in_outbreak))
  }
  data_agg
}

#' Filter Data Frame by Date Range
#'
#' This function filters a data frame to include only rows where a specified date column
#' falls within a given start and/or end date. The function accepts \code{Date} objects
#' or character strings (formatted as \code{"yyyy-mm-dd"}) for \code{date_start} and
#' \code{date_end} parameters.
#'
#' @param data A data frame containing the date column to filter by.
#' @param date_var A character string specifying the name of the date column in \code{data}.
#'   Default is \code{"date_report"}.
#' @param date_start A \code{Date} object or character string in \code{"yyyy-mm-dd"} format, or
#'   \code{NULL}. If provided, only rows where \code{date_var} is greater than or equal
#'   to \code{date_start} are included. Default is \code{NULL}.
#' @param date_end A \code{Date} object or character string in \code{"yyyy-mm-dd"} format, or
#'   \code{NULL}. If provided, only rows where \code{date_var} is less than or equal to
#'   \code{date_end} are included. Default is \code{NULL}.
#'
#' @return A filtered data frame containing only rows that match the specified date range.
#'
#' @examples
#' # Example data frame
#' \dontrun{
#' data <- data.frame(
#'   date_report = as.Date("2023-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#'
#' # Filter data from January 3, 2023 to January 8, 2023 (using Date format)
#' filtered_data <- filter_by_date(data,
#'   date_var = "date_report",
#'   date_start = as.Date("2023-01-03"),
#'   date_end = as.Date("2023-01-08")
#' )
#'
#' # Filter data using character format for dates
#' filtered_data <- filter_by_date(data,
#'   date_var = "date_report",
#'   date_start = "2023-01-03",
#'   date_end = "2023-01-08"
#' )
#' }
filter_by_date <- function(data, date_var = "date_report", date_start = NULL, date_end = NULL) {
  checkmate::check_subset(date_var, names(data))

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  if (!is.null(date_start)) {
    data <- data %>% dplyr::filter(!!rlang::sym(date_var) >= date_start)
  }
  if (!is.null(date_end)) {
    data <- data %>% dplyr::filter(!!rlang::sym(date_var) <= date_end)
  }
  data
}

#' Get Signals Stratified
#'
#' This function stratifies and aggregates surveillance data by specified columns and analyzes
#' each stratum separately using the specified method.
#'
#' @param data A data frame containing the surveillance data.
#' @param fun The signal detection function to apply to each stratum.
#' @param model character, default empty string which is the choice if farrington, ears or cusum are used and if a glm method was chosen as outbreak detection method then one of c("mean","sincos", "FN")
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param time_trend boolean default TRUE setting time_trend in the get_signals_glm(). This parameter is only used when an the glm based outbreak detection models are used, i.e. for the models c("mean","sincos", "FN")
#' @param stratification_columns A character vector specifying the columns to
#'   stratify the data by.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate signals for.
#' @return A tibble containing the results of the signal detection analysis
#'   stratified by the specified columns.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("../data/input/input.csv")
#' categories <- c("county", "sex", "age_group") # Replace with actual column names
#' results <- get_signals_stratified(
#'   data,
#'   fun = get_signals_farringtonflexible,
#'   stratification_columns = categories
#' )
#' print(results)
#' }
get_signals_stratified <- function(data,
                                   fun,
                                   model = "",
                                   intervention_date = NULL,
                                   time_trend = FALSE,
                                   stratification_columns,
                                   date_start = NULL,
                                   date_end = NULL,
                                   date_var = "date_report",
                                   number_of_weeks = 52) {
  # check that all columns are present in the data
  for (col in stratification_columns) {
    checkmate::assert(
      checkmate::check_choice(col, choices = names(data))
    )
  }

  checkmate::check_choice(model, choices = c("", "mean", "sincos", "FN"))

  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )

  checkmate::check_flag(time_trend)

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  # Initialize an empty list to store results per category
  category_results <- list()

  # get min and max date of the whole dataset before stratification
  # stratified aggregated data can be filled up with 0s until min and max date
  # of the full dataset
  if (is.null(date_start)) {
    date_start <- min(data[[date_var]], na.rm = TRUE)
  }
  if (is.null(date_end)) {
    date_end <- max(data[[date_var]], na.rm = TRUE)
  }

  i <- 0
  # Loop through each category
  for (category in stratification_columns) {
    if (is.factor(data[, category])) {
      # adding the NAs to also calculate signals for them
      strata <- levels(droplevels(addNA(data[, category], ifany = TRUE)))
    } else {
      strata <- unique(data[, category]) # character is supported as well
    }

    # iterate over all strata and run algorithm
    for (stratum in strata) {
      i <- i + 1
      # when stratum is NA filter needs to be done differently otherwise the NA stratum is lost
      if (is.na(stratum)) {
        sub_data <- data %>% dplyr::filter(is.na(.data[[category]]))
      } else {
        sub_data <- data %>% dplyr::filter(.data[[category]] == stratum)
      }

      sub_data_agg <- sub_data %>%
        # filter the data

        filter_by_date(date_var = date_var, date_start = date_start, date_end = date_end) %>%
        # aggregate data
        aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end)



      # run selected algorithm
      if (nrow(sub_data) == 0) {
        # don't run algorithm on those strata with 0 cases created by factors
        results <- sub_data_agg %>%
          # set alarms to FALSE for the timeperiod signals are generated for in the other present levels
          # logically the alarms column should also contain NA but later on computations are based on when the first alarm appears and when giving 0 timeseries to the algorithms they also put FALSE to the alarms column thus it is consistent
          # upperbound and expected to NA
          dplyr::mutate(alarms = dplyr::if_else(dplyr::row_number() > (nrow(.) - number_of_weeks + 1), FALSE, NA)) %>%
          dplyr::mutate(
            upperbound = NA,
            expected = NA
          )
      } else {
        if (model != "") {
          results <- fun(sub_data_agg, number_of_weeks, model = model, time_trend = time_trend, intervention_date = intervention_date)
        } else {
          results <- fun(sub_data_agg, number_of_weeks)
        }
      }

      if (is.null(results)) {
        warning(paste0(
          "The stratum ", category, ":", stratum,
          " will be neglected due to lack of data."
        ))
      } else {
        # add information on stratification to results
        results <- results %>% dplyr::mutate(
          category = category, stratum = stratum
        )
      }
      # Store the results in the list
      category_results[[i]] <- results
    }
  }

  return(dplyr::bind_rows(category_results))
}

#' Get Signals
#'
#' This function analyzes surveillance data to detect signals using the
#' specified method.
#'
#' @param data A data frame containing the surveillance data preprocessed with [preprocess_data()].
#' @param method The method to use for signal detection (currently supports
#'   "farrington").
#' @param intervention_date A date object or character of format yyyy-mm-dd specifying the date for the intervention in the pandemic correction models. After this date a new intercept and possibly time_trend is fitted.
#' @param stratification A character vector specifying the columns to stratify
#'   the analysis. Default is NULL.
#' @param date_start A date object or character of format yyyy-mm-dd specifying the start date to filter the data by. Default is NULL.
#' @param date_end A date object or character of format yyyy-mm-dd specifying the end date to filter the data by. Default is NULL.
#' @param date_var a character specifying the date variable name used for the aggregation. Default is "date_report".
#' @param number_of_weeks integer, specifying number of weeks to generate signals for.
#' @return A tibble containing the results of the signal detection analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- input_example %>%
#'   preprocess_data() %>%
#'   get_signals(
#'     method = "farrington",
#'     stratification = c("county", "sex")
#'   )
#' }
get_signals <- function(data,
                        method = "farrington",
                        intervention_date = NULL,
                        stratification = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        date_var = "date_report",
                        number_of_weeks = 52) {
  # check that input method and stratification are correct
  checkmate::assert(
    checkmate::check_choice(method, choices = available_algorithms())
  )

  checkmate::assert(
    checkmate::check_null(intervention_date),
    checkmate::check_date(lubridate::date(intervention_date)),
    combine = "or"
  )

  checkmate::assert(
    checkmate::check_null(stratification),
    checkmate::check_vector(stratification),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_character(date_var, len = 1, pattern = "date")
  )

  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )

  model <- ""
  time_trend <- FALSE


  if (method == "farrington") {
    fun <- get_signals_farringtonflexible
  } else if (method == "aeddo") {
    fun <- get_signals_aeddo
  } else if (method == "ears") {
    fun <- get_signals_ears
  } else if (method == "cusum") {
    fun <- get_signals_cusum
  } else if (grepl("glm", method)) {
    fun <- get_signals_glm
    if (method == "glm mean") {
      model <- "mean"
      time_trend <- FALSE
    } else if (method == "glm timetrend") {
      model <- "mean"
      time_trend <- TRUE
    } else if (method == "glm harmonic") {
      model <- "sincos"
      time_trend <- FALSE
    } else if (method == "glm harmonic with timetrend") {
      model <- "sincos"
      time_trend <- TRUE
    } else if (method == "glm farrington") {
      model <- "FN"
      time_trend <- FALSE
    } else if (method == "glm farrington with timetrend") {
      model <- "FN"
      time_trend <- TRUE
    }
  }

  if (is.null(stratification)) {

    data_agg <- data %>%
      # filter the data
      filter_by_date(date_start = date_start, date_end = date_end, date_var = date_var) %>%
      # aggregate and complete the data
      aggregate_data(date_var = date_var, date_start = date_start, date_end = date_end)

    if (grepl("glm", method)) {
      results <- fun(data_agg, number_of_weeks, model = model, time_trend = time_trend, intervention_date = intervention_date)
    } else {
      results <- fun(data_agg, number_of_weeks)
    }
    if (!is.null(results)) {
      results <- results %>%
        dplyr::mutate(category = NA, stratum = NA)
    }
  } else {
    results <- get_signals_stratified(
      data,
      fun,
      model = model,
      intervention_date = intervention_date,
      time_trend = time_trend,
      stratification,
      date_start,
      date_end,
      date_var,
      number_of_weeks
    )
  }

  # add number of weeks and method to the results dataframe
  if (!is.null(results)) {
    results <- results %>%
      dplyr::mutate(
        method = method,
        number_of_weeks = number_of_weeks
      )
  }


  return(results)
}

#' Get signals of surveillance's EARS algorithm
#' @param data_aggregated data.frame, aggregated data with case counts
#' @param number_of_weeks integer, specifying number of weeks to generate signals for
#' @param method string indicating which method to use: one of "C1", "C2", "C3"
#'
#' @examples
#' \dontrun{
#' data_aggregated <- input_example %>%
#'   preprocess_data() %>%
#'   aggregate_data() %>%
#'   add_rows_missing_dates()
#' results <- get_signals_ears(data_aggregated)
#' }
get_signals_ears <- function(data_aggregated,
                             number_of_weeks = 52,
                             method = "C1") {
  checkmate::assert(
    checkmate::check_integerish(number_of_weeks)
  )
  checkmate::assert(
    checkmate::check_choice(method, choices = c("C1", "C2", "C3"))
  )
  # using default value for baseline
  baseline <- 7

  sts_cases <- convert_to_sts(data_aggregated)

  num_weeks_total <- length(sts_cases@observed)
  num_weeks_for_calibration <- num_weeks_total - number_of_weeks

  if (num_weeks_for_calibration < 0) {
    warning(paste0(
      "The number of weeks you want to generate alarms for (n = ", number_of_weeks, ")",
      " is higher than the number of weeks you have in your data (n = ", num_weeks_total, ")."
    ))
    return(NULL)
  } else if (num_weeks_for_calibration < baseline) {
    warning(paste0(
      "Your data/stratification covers ",
      num_weeks_total,
      " number of weeks in total and you want to generate alarms for ", number_of_weeks, ". ",
      "EARS uses ", baseline, " weeks of data to calibrate an epidemiological basline. ",
      "You have ", num_weeks_for_calibration, " weeks for calibration left in your data/stratification."
    ))
    return(NULL)
  }


  control <- list(
    range = ((num_weeks_total - number_of_weeks + 1):num_weeks_total),
    method = method,
    baseline = baseline,
    minSigma = 0,
    alpha = 0.001
  )

  # run EARS on data
  results <- surveillance::earsC(sts_cases, control)

  pad <- rep(NA, num_weeks_total - number_of_weeks)
  alarms <- c(pad, results@alarm)
  upperbound <- c(pad, results@upperbound)

  data_aggregated$alarms <- alarms
  data_aggregated$upperbound <- upperbound
  # ears does not return an expected value
  data_aggregated$expected <- NA

  return(data_aggregated)
}

#' Removing columns from data which only contain missing values
#' @param data data.frame, dataset to remove empty columns from, can be linelist of surveillance data
#' @returns data.frame without columns which only contained missing values
remove_empty_columns <- function(data) {
  empty_columns <- get_empty_columns(data)
  empty_column_names <- names(empty_columns)[empty_columns]
  data %>%
    dplyr::select(-dplyr::all_of(empty_column_names))
}

#' Variable names of the variables which have yes, no , unknown levels
yes_no_unknown_variables <- function() {
  c(
    "hospitalization",
    "death",
    "vaccination",
    "outbreak_status"
  )
}

#' Variable names of the region_id variables including those which do not necessarily follow NUTS format
region_id_variable_names <- function() {
  c(
    "country_id",
    "state_id",
    "county_id",
    "community_id",
    "region_level1_id",
    "region_level2_id",
    "region_level3_id"
  )
}

#' Varible names which should be checked for missing values
check_for_missing_values <- function() {
  c("date_report")
}

#' Allowed levels for variables with yes, no, unknown levels in preprocessed surveillance data used for calculations
yes_no_unknown_levels <- function() {
  c(
    "yes",
    "no",
    NA_character_
  )
}

#' Creates age grouping variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input_sample.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data$age <- sample(1:125, 10, replace = TRUE)
#' age_groups(data) # default age groups
#' age_groups(data, c(15L, 35L, 65L, 100L)) # custom age groups
#' }
age_groups <- function(df, break_at = NULL) {
  # error checking ----------------------------------------------------------

  # check whether age_groups already exist
  if (!("age_group" %in% colnames(df))) {
    # if age_group doesn't exist, create it from age
    if (!is.null(break_at)) { # check for non integer values
      if (!(is.integer(break_at))) {
        stop("Input of integer type is only allowed")
      }

      var <- length(break_at) # helper vector
      for (i in 1:(var - 1)) { # check if break points are ordered
        if (break_at[i + 1] < break_at[i]) {
          stop("Invalid break points")
        }
      }
    }

    # setting up age groups ---------------------------------------------------
    default_break_at <- seq(5, 125, 5)

    if (is.null(break_at)) { # use default age groups
      set <- c(0, default_break_at) # helper vector
    } else { # use custom age groups
      set <- c(0, break_at)
    }

    if (!checkmate::test_integerish(df$age)) { # check for integer, it is sufficient that they are whole numbers does not need to be of type integer, i.e. is.integer(3) would give FALSE
      stop("Type of age is not integer")
    }

    # assigning age group  ----------------------------------------------------
    for (i in 1:nrow(df)) { # assign age group to every age in data frame
      df$age_group[i] <- find_age_group(df$age[i], set)
    }

    # move age_group to correct position
    df <- df %>% dplyr::relocate(age_group, .after = age)
  }

  # conducting format enquires
  format_check_results <- age_format_check(df)

  # if not the correct xx-xx format is used, insert leading 0's where necesary
  if (length(format_check_results$format_agegrp_xx) > 0) {
    splits <- stringr::str_split_fixed(as.character(df$age_group), format_check_results$agegrp_div, 2)

    for (item in format_check_results$format_agegrp_xx) {
      df$age_group[item] <- paste0(
        sprintf("%02d", as.numeric(splits[item, 1])),
        format_check_results$agegrp_div,
        sprintf("%02d", as.numeric(splits[item, 2]))
      )
    }
  }

  all_agegroups <- complete_agegrp_arr(df, format_check_results)

  # store the age groups in the environment
  app_cache_env$age_group_levels <- stringr::str_sort(all_agegroups, numeric = TRUE)

  # converting age_group to factor ------------------------------------------
  df$age_group <- factor(df$age_group,
                         levels = app_cache_env$age_group_levels
  )

  return(df)
}


# This file should include all relevant helper functions required to run the actual signal detection tool
# Feel free to complete the file

#' Finds correct age interval for given age
#' @param age integer age in years
#' @param x vector of age group break points
#'
#' @examples
#' \dontrun{
#' find_age_group(5, c(0, 5, 10, 99)) # would result in "05-09"
#' find_age_group(12, c(0, 5, 15, 99)) # would result in "05-14"
#' find_age_group(NA, c(0, 5, 15, 99)) # would result in NA
#' }
find_age_group <- function(age, x) {
  intervals <- length(x) # number of age groups

  for (i in 1:intervals) { # finding interval in which age lies
    if (i == intervals) { # check if last age group
      group <- paste0(x[i], "+")
      return(group)
    }
    if (is.na(age)) {
      group <- NA_character_
      return(group)
    } else {
      if ((x[i] <= age) & (age < x[i + 1])) {
        if (age < 10 | x[i] < 10) { # zero padding
          group <- paste(paste0(0, x[i]),
                         ifelse(x[i + 1] - 1 < 10,
                                paste0(0, x[i + 1] - 1),
                                x[i + 1] - 1
                         ),
                         sep = "-"
          )
          return(group)
        } else {
          group <- paste(x[i], x[i + 1] - 1, sep = "-")
          return(group)
        }
      }
    }
  }
}

#' Creation of age_group levels from different formats of the age_group column
#'
#' This function returns a character vector with age_group levels based on the data provided.
#' It uses the age_format_check() and complete_agegrp_arr() functions to create the levels
#'
#' @param df A data frame containing an 'age_group' variable.
#' @return character vector containing all age_group levels
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 6, 16), age_group = c("01-05", "6-10", "16-20"))
#' create_age_group_levels(data_frame)
#' }
create_age_group_levels <- function(df) {
  format_check_results <- age_format_check(df)
  all_agegroups <- complete_agegrp_arr(df, format_check_results)
  age_group_levels <- stringr::str_sort(all_agegroups, numeric = TRUE)

  age_group_levels
}
#' Age Group Format Check
#'
#' This function checks the format of the 'age_group' variable in the given data frame. It performs several checks including:
#'   1. Checking if the lengths of age groups are equidistant.
#'   2. Verifying if the age group format is in the "xx-xx" format.
#'   3. Checking if any punctuation characters are used at either end of the age group.
#'
#' @param df A data frame containing an 'age_group' variable.
#'
#' @return A list containing the results of the formatting checks:
#'   \item{agegrp_div}{The most frequently used punctuation character, which serves as the divider in age groups.}
#'   \item{other_punct_character}{Any other punctuation character used. Also used as logical indicator.}
#'   \item{equal_sizing}{Logical indicating whether the lengths of age groups are equidistant.}
#'   \item{format_agegrp_xx}{Indices of entries in 'age_group' not following the "xx-xx" format.}
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 5, 15, 16), age_group = c("01-05", "6-10", "11-15", "16-20"))
#' check_results <- age_format_check(data_frame)
#' print(check_results)
#' }
age_format_check <- function(df) {
  # setting variables
  splits_uniq <- stringr::str_split_fixed(as.character(unique(df$age_group)), "[^[:alnum:]]", 2)
  splits_total <- stringr::str_split_fixed(as.character(df$age_group), "[^[:alnum:]]", 2)
  min_num <- as.numeric(splits_uniq) %>%
    stats::na.omit() %>%
    min()
  max_num <- as.numeric(splits_uniq) %>%
    stats::na.omit() %>%
    max()

  # checking if length is equidistant
  abs_diff <- abs(as.numeric(splits_uniq[, 1]) - as.numeric(splits_uniq[, 2])) %>% stats::na.omit()
  equal_sizing <- (length(unique(abs_diff)) == 1)

  # checking whether xx-xx format is in use
  tmp_check <- union(
    which(stringr::str_length(splits_total[, 1]) < 2),
    which(stringr::str_length(splits_total[, 2]) < 2)
  )

  format_agegrp_xx <-
    setdiff(
      tmp_check,
      which(splits_total == "", arr.ind = TRUE)[, 1]
    )

  # extract which divider is used
  agegrp_div <- stringr::str_match(
    string = as.character(unique(df$age_group)),
    pattern = "\\d+(.*?)\\d+"
  )[, 2] %>%
    stringr::str_subset(".+") %>%
    unique()

  # extract which other special characters are used and how and where
  tmp_start <- stringr::str_extract(
    string = stringr::str_sort(unique(df$age_group), numeric = TRUE),
    pattern = "(^[^[:alnum:]])"
  )

  tmp_end <- stringr::str_extract(
    string = stringr::str_sort(unique(df$age_group), numeric = TRUE),
    pattern = "([^[:alnum:]]$)"
  )
  tmp_df <- data.frame(tmp_start, tmp_end)

  other_punct_char <- list()
  for (df_col in 1:ncol(tmp_df)) {
    for (item in purrr::discard(tmp_df[, df_col], is.na)) {
      num_val <- stringr::str_extract(
        string  = grep(pattern = paste0("[\\", item, "]"), x = unique(df$age_group), value = TRUE),
        pattern = "\\d+"
      )

      other_punct_char[[item]] <- list(
        char_val = item,
        num_val = num_val,
        placement_in_arr = ifelse(num_val == min_num, "start", "end"),
        placement_in_str = ifelse(df_col == 1, "start", "end")
      )
    }
  }

  # return list of formatting checks results
  return(list(
    agegrp_div = agegrp_div,
    other_punct_char = other_punct_char,
    equal_sizing = equal_sizing,
    format_agegrp_xx = format_agegrp_xx
  ))
}


#' Complete Age Group Array
#'
#' This function generates a complete array of age groups based on the format check results and existing age group data.
#' It is particularly useful for completing missing age groups in datasets
#'
#' @param df A data frame containing an 'age_group' variable.
#' @param format_check_results A list containing the results of the age group format check.
#'
#' @return A character vector representing the complete array of age groups.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data_frame <- data.frame(age = c(2, 5, 15, 16), age_group = c("01-05", "6-10", "11-15", "16-20"))
#' format_check_results <- list(equal_sizing = TRUE, agegrp_div = "-", other_punct_char = list(), format_agegrp_xx = 2)
#' complete_agegrp_arr(data_frame, format_check_results)
#' }
complete_agegrp_arr <- function(df, format_check_results) {
  # find unique elements of age_group
  # remove NA from the unique age_groups for the whole process and add it later again
  tmp_uniq_agegrp <- stringr::str_sort(unique(df$age_group), numeric = TRUE)
  tmp_uniq_agegrp <- stats::na.omit(tmp_uniq_agegrp)


  # check to see if there are any gaps
  # building regex string dependent on level of punct characters found
  regex_string <-
    paste0(
      "[\\",
      gsub(x = format_check_results$agegrp_div, pattern = " ", replacement = ""),
      ""
    )

  if (length(format_check_results$other_punct_char) > 0) {
    for (character in names(format_check_results$other_punct_char)) {
      regex_string <- paste0(regex_string, "\\", gsub(x = character, pattern = " ", replacement = ""))
    }
  }

  regex_string <- paste0(regex_string, "]")

  # splitting the ages, only keeping numeric
  splits <- stringr::str_split_fixed(
    string = tmp_uniq_agegrp,
    pattern = regex_string,
    n = 2
  ) %>%
    gsub(pattern = "\\D", replacement = "")

  # remove rows of only NA or empty
  splits <- splits[!apply(is.na(splits) | splits == "", 1, all), ]

  # if there is equidistance
  if (format_check_results$equal_sizing) {
    # create sequence based on existing age_groups
    # find an example of an age group that is not NULL
    tmp_agegrp <- tmp_uniq_agegrp[grepl(paste0("[\\", format_check_results$agegrp_div, "]"), tmp_uniq_agegrp)][1]

    # get the lower and upper of the age group gap using the first age_group and find the range
    lower_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp), format_check_results$agegrp_div)[1])
    upper_split <- as.numeric(stringr::str_split_1(as.character(tmp_agegrp), format_check_results$agegrp_div)[2])
    split_range <- plyr::round_any(upper_split - lower_split, accuracy = 5)

    if ("age" %in% colnames(df)) {
      # find the maximum age in relation to the age group gap
      max_age <- max(df$age, na.rm = T)
      max_data_rounded <- plyr::round_any(
        x = max_age,
        accuracy = split_range,
        f = ceiling
      )
    } else {
      # age is not mandatory also only age_group can be in the dataset
      # then take the largest age_group as maximum age
      max_data_rounded <- as.numeric(max(splits))
      max_age <- max_data_rounded
    }


    # make sequences
    seq_lower <- seq(0, max_data_rounded, split_range)
    seq_upper <- (seq_lower - 1)[-1]
    if (max_age >= seq_lower[length(seq_lower)]) {
      # add the last missing upper seq which is lost and needed because we have max_age in this age_group
      seq_upper <- c(seq_upper, seq_upper[length(seq_upper)] + split_range)
    }
  } else if (!format_check_results$equal_sizing) {
    # find where there are missing gaps
    dummy_fill <- data.frame(
      lower = 1:length(tmp_uniq_agegrp) * NA,
      upper = 1:length(tmp_uniq_agegrp) * NA
    )

    for (i in 2:length(splits[, 1])) {
      # compare the start of the next split with the end of the previous to identify gaps
      if (as.numeric(splits[i, 1]) - as.numeric(splits[i - 1, 2]) > 1) {
        dummy_fill[i, 1] <- as.numeric(splits[i - 1, 2])
        dummy_fill[i, 2] <- as.numeric(splits[i, 1])
      }
    }

    dummy_fill <- dummy_fill %>% dplyr::filter(!is.na(lower))

    # create the sequence levels
    # if new age groups were created add them and put them in the correct order
    seq_lower <- c(splits[, 1], (dummy_fill$lower + 1)) %>%
      stringr::str_sort(., numeric = TRUE) %>%
      as.numeric()
    seq_upper <- c(splits[, 2], (dummy_fill$upper - 1)) %>%
      .[nzchar(., keepNA = FALSE)] %>%
      stringr::str_sort(., numeric = TRUE) %>%
      as.numeric()
  }

  # correcting for NAs
  if (any(is.na(seq_lower))) {
    if (which(is.na(seq_lower)) == 1) {
      seq_lower[1] <- 0
    }
    seq_lower <- stats::na.omit(seq_lower)
  }

  if (any(is.na(seq_upper))) {
    seq_upper <- stats::na.omit(seq_upper)
  }

  # ensuring lower and upper seq are equal in length
  if (!(length(seq_lower) == length(seq_upper))) {
    if (length(seq_lower) > length(seq_upper)) {
      seq_lower <- seq_lower[-length(seq_lower)]
    } else {
      seq_upper <- seq_upper[-length(seq_upper)]
    }
  }

  # combine to create the complete age group array
  # adding leading zeros on <10 ages
  all_agegrps <- paste0(
    sprintf("%02d", seq_lower),
    format_check_results$agegrp_div,
    sprintf("%02d", seq_upper)
  )

  # if symbol is used, add it where appropriate
  # make complete string
  if (length(format_check_results$other_punct_char) > 0) {
    max_num_inp <- NULL
    for (spc_char in names(format_check_results$other_punct_char)) {
      # if it belong in front of or behind value
      if (format_check_results$other_punct_char[[spc_char]]$placement_in_str == "start") {
        str_element_1 <- format_check_results$other_punct_char[[spc_char]]$char_val
        str_element_2 <- format_check_results$other_punct_char[[spc_char]]$num_val
      } else {
        str_element_1 <- format_check_results$other_punct_char[[spc_char]]$num_val
        str_element_2 <- format_check_results$other_punct_char[[spc_char]]$char_val
      }

      agegroup_str <- paste0(str_element_1, str_element_2)

      # if it is part of the first or the last number
      if (format_check_results$other_punct_char[[spc_char]]$placement_in_arr == "start") {
        all_agegrps[1] <- agegroup_str
      } else {
        all_agegrps[length(all_agegrps) + 1] <- agegroup_str
        max_num_inp <- format_check_results$other_punct_char[[spc_char]]$num_val
      }
    }

    # remove potential numbers smaller and larger than number with special character attached
    if (!is.null(max_num_inp)) {
      # splitting the ages, only keeping numeric
      splits_tmp <- stringr::str_split_fixed(
        string = all_agegrps,
        pattern = regex_string,
        n = 2
      ) %>%
        gsub(pattern = "\\D", replacement = "")

      rows_to_remove <- which(apply(splits_tmp, 2, as.numeric) > as.numeric(max_num_inp), arr.ind = T) %>%
        as.data.frame() %>%
        dplyr::select(row) %>%
        dplyr::distinct() %>%
        unlist()

      all_agegrps <- all_agegrps[-rows_to_remove]
    }
  }
  # adding back NAs when there were any
  if (any(is.na(df$age_group))) {
    all_agegrps <- c(all_agegrps, NA_character_)
  }

  # checking to see if all agegroups entities are found in the array
  # if not all are present just use the original age_groups in the data
  if (any(unique(df$age_group) %in% all_agegrps == FALSE)) {
    all_agegrps <- unique(df$age_group)
  }

  return(all_agegrps)
}

#' Creates age grouping variable for a given data set
#' @param df data frame on which the age grouping is created
#' @param break_at integer that controls the length of the age groups
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input_sample.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data$age <- sample(1:125, 10, replace = TRUE)
#' age_groups(data) # default age groups
#' age_groups(data, c(15L, 35L, 65L, 100L)) # custom age groups
#' }
age_groups <- function(df, break_at = NULL) {
  # error checking ----------------------------------------------------------

  # check whether age_groups already exist
  if (!("age_group" %in% colnames(df))) {
    # if age_group doesn't exist, create it from age
    if (!is.null(break_at)) { # check for non integer values
      if (!(is.integer(break_at))) {
        stop("Input of integer type is only allowed")
      }

      var <- length(break_at) # helper vector
      for (i in 1:(var - 1)) { # check if break points are ordered
        if (break_at[i + 1] < break_at[i]) {
          stop("Invalid break points")
        }
      }
    }

    # setting up age groups ---------------------------------------------------
    default_break_at <- seq(5, 125, 5)

    if (is.null(break_at)) { # use default age groups
      set <- c(0, default_break_at) # helper vector
    } else { # use custom age groups
      set <- c(0, break_at)
    }

    if (!checkmate::test_integerish(df$age)) { # check for integer, it is sufficient that they are whole numbers does not need to be of type integer, i.e. is.integer(3) would give FALSE
      stop("Type of age is not integer")
    }

    # assigning age group  ----------------------------------------------------
    for (i in 1:nrow(df)) { # assign age group to every age in data frame
      df$age_group[i] <- find_age_group(df$age[i], set)
    }

    # move age_group to correct position
    df <- df %>% dplyr::relocate(age_group, .after = age)
  }

  # conducting format enquires
  format_check_results <- age_format_check(df)

  # if not the correct xx-xx format is used, insert leading 0's where necesary
  if (length(format_check_results$format_agegrp_xx) > 0) {
    splits <- stringr::str_split_fixed(as.character(df$age_group), format_check_results$agegrp_div, 2)

    for (item in format_check_results$format_agegrp_xx) {
      df$age_group[item] <- paste0(
        sprintf("%02d", as.numeric(splits[item, 1])),
        format_check_results$agegrp_div,
        sprintf("%02d", as.numeric(splits[item, 2]))
      )
    }
  }

  all_agegroups <- complete_agegrp_arr(df, format_check_results)

  # store the age groups in the environment
  app_cache_env$age_group_levels <- stringr::str_sort(all_agegroups, numeric = TRUE)

  # converting age_group to factor ------------------------------------------
  df$age_group <- factor(df$age_group,
                         levels = app_cache_env$age_group_levels
  )

  return(df)
}

#' Allowed levels for sex in preprocessed surveillance data used for all calculations
sex_levels <- function() {
  app_cache_env$sex_levels
}

#' Turns aggregated data into surveillance's sts format
#' @param case_counts case count data frame to be converted
#'
#' @examples
#' \dontrun{
#' input_path <- "data/input/input.csv"
#' data <- read.csv(input_path, header = TRUE, sep = ",")
#' data <- preprocess_data(data) %>% aggregate_data()
#' sts_cases <- convert_to_sts(data)
#' }
convert_to_sts <- function(case_counts) {
  # create sts object
  return(surveillance::sts(case_counts$cases,
                           start = c(
                             case_counts$year[1],
                             case_counts$week[1]
                           ),
                           frequency = 52
  ))
}

available_algorithms <- function() {
  c(
    "FarringtonFlexible" = "farrington",
    "EARS" = "ears",
    "CUSUM" = "cusum",
    "Mean" = "glm mean",
    "Timetrend" = "glm timetrend",
    "Harmonic" = "glm harmonic",
    "Harmonic with timetrend" = "glm harmonic with timetrend",
    "Step harmonic" = "glm farrington",
    "Step harmonic with timetrend" = "glm farrington with timetrend"
  )
}



#' Retrieveing which columns in the dataset only contain missing values
#' @param data data.frame, dataset to check for empty columns can be linelist of surveillance data
#' @returns named vector with column names and boolean specifying complete missingness or not
get_empty_columns <- function(data) {
  apply(data, 2, function(x) {
    (all(is.na(x)) | all(x == "") | all(x == "unknown") | all(x == "NA"))
  })
}

#' Add missing isoweeks to an aggregated dataframe of case counts by year and week
#'
#' This function takes a data frame containing year-week and case count and ensures
#' that it includes all possible year-week combinations within the specified
#' range. It fills in missing rows with 0 values for cases and returns the
#' updated data frame.
#'
#' @param data_agg An aggregated data frame containing at least the columns 'year','week','cases' representing the isoyear, isoweek and case counts.
#' @param date_start A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the minimum date of the dataset. This parameter can be used when the dataset should be extended until the date_start provided.
#' @param date_end A date object or character of format yyyy-mm-dd. Default is NULL which means that missing isoweeks are added until the maximum date of the dataset. This can be used when the dataset should be extended until the date_end provided.
#' @return A data frame containing all year-week combinations within the input
#'   range, with previously missing year-weeks filled in with 0 values for cases.
#'
#' @examples
#' \dontrun{
#' data_agg <- data.frame(
#'   year = c(2021, 2022, 2022),
#'   week = c(1, 2, 4),
#'   cases = c(10, 15, 5)
#' )
#' updated_data <- add_missing_isoweeks(data_agg, "2022-01-21", "2023-05-01")
#' updated_data <- add_missing_isoweeks(data_agg)
#' updated_data
#' }
#' @export
add_missing_isoweeks <- function(data_agg, date_start = NULL, date_end = NULL) {
  checkmate::assert(
    checkmate::check_subset("year", names(data_agg)),
    checkmate::check_subset("week", names(data_agg)),
    checkmate::check_subset("cases", names(data_agg)),
    combine = "and"
  )

  checkmate::assert(
    checkmate::check_null(date_start),
    checkmate::check_date(lubridate::date(date_start)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_null(date_end),
    checkmate::check_date(lubridate::date(date_end)),
    combine = "or"
  )

  # add a date based on isoweek and isoyear
  data_agg <- data_agg %>%
    dplyr::mutate(date = isoweek_to_date(week, year))

  # extend the dataset nevertheless to min date if date_start greater
  min_date <- min(data_agg$date)
  if (is.null(date_start)) {
    date_start <- min_date
  } else if (date_start > min_date) {
    message("Notice: Your input date_start is greater than the smallest date in the dataset. Missing weeks are nevertheless filled until the smallest date in the dataset")
    date_start <- min_date
  }
  # extend the dataset nevertheless to max date if date_end is smaller
  max_date <- max(data_agg$date)
  if (is.null(date_end)) {
    date_end <- max_date
  } else if (date_end < max_date) {
    message("Notice: Your input date_end is smaller than the greatest date in the dataset. Missing weeks are nevertheless filled until the greatest date in the dataset")
    date_end <- max_date
  }

  # Generate a sequence of dates from start to end date
  # we add the date_end because the seq ends before date_end when there is a partial week remaining and we also want to have the isoweek of the date_end
  date_seq <- c(seq.Date(from = as.Date(date_start), to = as.Date(date_end), by = "week"), date_end)
  # Create a data frame with ISO weeks and ISO years
  df_all_years_weeks <- data.frame(
    year = lubridate::isoyear(date_seq),
    week = lubridate::isoweek(date_seq)
  ) %>%
    # in case date_end is there twice due to adding before
    dplyr::distinct(year, week)


  # Merge the template with the original data to fill in missing rows
  data_agg_complete <- merge(data_agg, df_all_years_weeks, by = c("year", "week"), all = TRUE) %>%
    dplyr::select(-date)
  # Replace missing cases with 0
  data_agg_complete$cases[is.na(data_agg_complete$cases)] <- 0

  data_agg_complete <- data_agg_complete %>%
    dplyr::arrange(year, week)

  return(data_agg_complete)
}

#' Create a Date from ISO Year and Week
#'
#' This function converts an ISO year and ISO week number into a date. The date
#' returned corresponds to the first day (Monday) of the specified ISO week.
#'
#' @param week Integer. The ISO week number (1 to 53).
#' @param year Integer. The ISO year.
#' @return A `Date` object representing the first day of the specified ISO week.
#'
#' @examples \dontrun{
#' Example usage:
#' isoweek_to_date(week = 15, year = 2023)  # Returns the date for the first day of ISO week 15 in 2023
#' }
isoweek_to_date <- function(week, year) {
  # Format the ISO week string
  iso_week_str <- sprintf("%d-W%02d-1", year, week)

  # Convert to date (the first day of the ISO week)
  date <- ISOweek::ISOweek2date(iso_week_str)
  return(date)
}

#' Plot time-series based on the results of a signal detection algorithm, being alarms, threshold and expectation
#'
#' Static plots (default) are only based on the dates of the latest
#' `number_of_weeks` weeks. Interactive plots are based on all data, but zoom in
#' by default on the latest `number_of_weeks` weeks.
#'
#' @param results data returned by the get_signals_farringtonflexible()
#' @param interactive logical, if TRUE, interactive plot is returned; default, static plot.
#' @param intervention_date A date object or character of format yyyy-mm-dd or NULL specifying the date for the intervention in the pandemic correction models. Default is NULL which indicates that no intervention is done.The  intervention is marked with a dashed line.
#' @param number_of_weeks number of weeks to be covered in the plot
#'
#' @return either a gg or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read.csv("data/input/input.csv", header = TRUE, sep = ",")
#' results <- get_signals_farringtonflexible(data)
#' plot_time_series(results)
#' }
plot_time_series <- function(results, interactive = FALSE,
                             intervention_date = NULL,
                             number_of_weeks = 52) {
  # check whether timeseries contains padding or not
  padding_upperbound <- "upperbound_pad" %in% colnames(results)
  padding_expected <- "expected_pad" %in% colnames(results)
  padding <- any(padding_expected, padding_upperbound)

  results <- results %>%
    dplyr::mutate(
      isoweek = paste0(
        .data$year, "-W",
        stringr::str_pad(.data$week, width = 2, pad = "0")
      ),
      date = ISOweek::ISOweek2date(paste0(.data$isoweek, "-1")),
      set_status = dplyr::if_else(is.na(.data$alarms), "Training data", "Test data"),
      set_status = factor(.data$set_status, levels = c("Training data", "Test data"))
    )

  if (padding_upperbound) {
    results <- results %>%
      dplyr::mutate(hover_text = paste0(
        ifelse(.data$set_status == "Test data", "Signal detection period", ""),
        "<br>Week: ", .data$isoweek,
        "<br>Observed: ", .data$cases,
        ifelse(!is.na(.data$upperbound_pad) | !is.na(.data$upperbound), (
          ifelse(is.na(.data$upperbound_pad),
                 paste0("<br>Threshold: ", round(.data$upperbound, 1)),
                 paste0("<br>Threshold: ", round(.data$upperbound_pad, 1))
          )
        ), ""),
        ifelse(!is.na(.data$expected_pad) | !is.na(.data$expected), (
          ifelse(is.na(.data$expected_pad),
                 paste0("<br>Expected: ", round(.data$expected, 1)),
                 paste0("<br>Expected: ", round(.data$expected_pad, 1))
          )
        ), "")
      ))
  } else {
    results <- results %>%
      dplyr::mutate(hover_text = paste0(
        ifelse(.data$set_status == "Test data", "Signal detection period", ""),
        "<br>Week: ", .data$isoweek,
        "<br>Observed: ", .data$cases,
        ifelse(!is.na(.data$upperbound),
               paste0("<br>Threshold: ", round(.data$upperbound, 1)), ""
        ),
        ifelse(!is.na(.data$expected),
               paste0("<br>Expected: ", round(.data$expected, 1)), ""
        )
      ))
  }


  # Periods - ends on the first date in the following week, [start; end)
  # Dates for the latest ~year (`number_of_weeks` period).
  range_dates_year <- max(results$date) - lubridate::weeks(c(number_of_weeks, 0) - 1)

  # Static plots should be based only on the latest `number_of_weeks` weeks
  if (!interactive) {
    results <- results %>%
      dplyr::filter(.data$date >= .env$range_dates_year[1])
  }

  # Dates for the training period and _signal _detection _period (test data period)
  period_dates_df <- results %>%
    dplyr::group_by(.data$set_status) %>%
    dplyr::summarise(
      start = min(.data$date),
      end = max(.data$date) + lubridate::days(7)
    )
  # number of days in _signal _detection _period
  ndays_sdp <- dplyr::filter(
    period_dates_df,
    .data$set_status == "Test data"
  ) %>%
    {
      difftime(.$end, .$start, units = "days")
    } %>%
    as.numeric()
  # Add dummy week to `results` to end the threshold line by a
  #   horizontal segment (geom_step) in the final week
  results <- results %>%
    dplyr::filter(date == max(.data$date)) %>% # final week-date
    dplyr::mutate(
      cases = NA, alarms = NA,
      date = .data$date + lubridate::days(7),
      hover_text = "" # don't show misleading hover at dummy data
    ) %>%
    dplyr::bind_rows(results, .)

  # function to find a nice-looking ymax value for y-axis range
  #   (plotly does not work with ymax=Inf)
  custom_round_up <- function(x) max(pretty(c(0, x)))
  # compute local ymax for plotly adaptive y-axis when zooming in on x-axis
  if (padding_upperbound) {
    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ymax = custom_round_up(c(
        .data$cases * dplyr::if_else(.data$alarms, 1.1, 1, missing = 1),
        # 1.1 to add space for signal-* on top-edge of case-number bars
        .data$upperbound, .data$upperbound_pad, 1
      )))
  } else {
    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ymax = custom_round_up(c(
        .data$cases * dplyr::if_else(.data$alarms, 1.1, 1, missing = 1),
        # 1.1 to add space for signal-* on top-edge of case-number bars
        .data$upperbound, 1
      )))
  }

  # ymax overall for rectangle background
  ymax_data <- max(results$ymax)

  col.threshold <- "#2297E6"
  col.expected <- "#000000"
  col.alarm <- "#FF0000"
  col.training <- "#9E9E9E"
  col.test <- "#304794"
  col.intervention <- "#ff8c00"

  legend_values <- c(
    "Expected" = col.expected,
    "Threshold" = col.threshold
  )

  half_week <- lubridate::days(3)

  plt <-
    results %>%
    ggplot2::ggplot(ggplot2::aes(x = date, group = 1, text = hover_text)) +
    ggplot2::geom_rect(
      data = period_dates_df, inherit.aes = FALSE,
      ggplot2::aes(
        x = NULL, y = NULL,
        xmin = start, xmax = end,
        fill = paste0("bg_", set_status)
      ),
      ymin = 0, ymax = ymax_data,
      colour = "white", linewidth = 0.5, alpha = 0.2
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        x = date + half_week, # center bars around mid-week
        y = cases, fill = set_status
      )
    ) +
    ggplot2::geom_step(ggplot2::aes(y = upperbound, color = "Threshold"),
                       linewidth = 1.3, direction = "hv"
    )

  if (padding_upperbound && any(!is.na(results$upperbound_pad))) {
    plt <- plt +
      ggplot2::geom_step(
        ggplot2::aes(y = upperbound_pad, color = "Threshold", linetype = "Test data"),
        linewidth = 0.3, direction = "hv"
      )
  }

  if (padding_expected && any(!is.na(results$expected_pad))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
                         linewidth = 1.3, direction = "hv"
      ) +
      ggplot2::geom_step(ggplot2::aes(y = expected_pad, color = "Expected", linetype = "Training data"),
                         linewidth = 0.3, direction = "hv"
      )
  } else if (any(!is.na(results$expected))) {
    plt <- plt +
      ggplot2::geom_step(ggplot2::aes(y = expected, color = "Expected"),
                         linewidth = 1.3, direction = "hv"
      )
  }

  # adding intervention vertical line
  if (!is.null(intervention_date)) {
    plt <- plt +
      ggplot2::geom_vline(xintercept = intervention_date, linetype = "dashed", color = col.intervention, size = 0.7)
    legend_values <- c(legend_values, "Intervention" = col.intervention)
  }

  # adding signal points
  plt <- plt +
    ggplot2::geom_point(
      data = dplyr::filter(results, alarms == TRUE),
      ggplot2::aes(x = date + half_week, y = cases, shape = alarms, stroke = 1),
      color = col.alarm, size = 6
    )

  plt <- plt +
    ggplot2::scale_x_date(
      date_breaks = "month", date_labels = "%Y-%m-%d",
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = 5),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 8),
      labels = c("TRUE" = "Signal")
    ) +
    ggplot2::scale_color_manual(values = legend_values) +
    ggplot2::scale_fill_manual(
      values = c(
        "Test data" = col.test, "Training data" = col.training,
        "bg_Training data" = "white", "bg_Test data" = col.threshold
      ),
      labels = c("Test data" = "Signal detection period"),
      breaks = c("Test data")
    ) +
    ggplot2::scale_linetype_manual(values = c("Training data" = 1, "Test data" = 1), name = "", guide = "none") +
    ggplot2::theme(
      legend.position = "top",
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey75"),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks.length.x = ggplot2::unit(0.25, "cm"),
      axis.text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, vjust = 1),
      axis.title.x = ggplot2::element_text(face = "bold", size = 14),
      axis.title.y = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2),
      shape = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::labs(
      x = "Time",
      y = "Number of infected",
      color = NULL,
      fill = NULL,
      shape = NULL
    )

  if (interactive) {
    range_dates_all <- range(results$date)
    plt <- plotly::ggplotly(plt, tooltip = "text", dynamicTicks = TRUE) %>%
      plotly::layout(
        xaxis = list(
          type = "date",
          autorange = FALSE,
          range = range_dates_year,
          rangeslider = list(
            range = range_dates_all,
            visible = TRUE,
            yaxis = list(
              range = c(0, ymax_data),
              rangemode = "fixed"
            ), # so we always can see the big picture in the rangeslider plot
            thickness = 0.10
          ),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
              list(step = "all", label = "All time points")
            )
          )
        ),
        yaxis = list(range = c(
          0,
          results %>%
            # pick the default x-range view
            dplyr::filter(date >= range_dates_year[1]) %>%
            dplyr::select("ymax") %>% max()
        )),
        legend = list(
          orientation = "h", x = 0.5, y = -0.9,
          yanchor = "top", xanchor = "center"
        )
      ) %>%
      plotly::config(modeBarButtonsToRemove = c(
        "autoScale2d",
        "select2d",
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d",
        "pan2d",
        "zoom2d",
        "toggleSpikelines"
      ))

    # JavaScript callback function using htmlwidgets::onRender() to listen for
    # the plotly_relayout event when x-axis range is adjusted.
    # Since it is client-side also works in HTML-reports. Two purposes:
    # 1. Dynamically adapt ymax of the interactive plot based on the x-axis zoom.
    # 2. Fix to plotly rangeselector step="all", which extends x-axis into
    ##   the future, when signal markers are present. See
    ##   https://github.com/United4Surveillance/signal-detection-tool/issues/231
    update_axes <- function(plot) {
      htmlwidgets::onRender(plot, "
          function(el, x, jsondata) {
            el.on('plotly_relayout', function(eventdata) {
              var x_autorange = eventdata['xaxis.autorange'];
              if(x_autorange === true) {
                // correct possible plotly-auto-extended x-axis
                // use a copy of full date range to avoid modification
                var data_xrange = [...jsondata['date_range']];
                Plotly.relayout(el, {'xaxis.rangeslider.range': data_xrange,
                                     'xaxis.range': data_xrange});
              }

              var x_range = eventdata['xaxis.range'];
                // undefined when x_autorange is true
              if(x_range) {
                // adapt ymax on y-axis to zoomed data
                var x_min = x_range[0];
                var x_max = x_range[1];
                var max_y_in_view =
                  Math.max.apply(null, jsondata['results'].filter(function(d) {
                      return d.date >= x_min && d.date <= x_max;
                    }).map(d => d.ymax)
                  );
                Plotly.relayout(el, {'yaxis.range': [0, max_y_in_view]});
              }
            });
          }
      ", data = list(
        results = dplyr::select(results, c("date", "ymax")),
        date_range = range_dates_all
      ))
    }
    # Update the plot with dynamic y-axis adjustment and x-axis bugfix
    plt <- update_axes(plt)

    # modifying the interactive plot legend
    # This is horrible, we need to find a solution at some point to do this differently
    plt$x$data[[1]]$showlegend <-
      plt$x$data[[2]]$showlegend <- FALSE
    plt$x$data[[1]]$hoverinfo <-
      plt$x$data[[2]]$hoverinfo <- "skip"
    plt$x$data[[3]]$showlegend <- FALSE
    plt$x$data[[4]]$name <- plt$x$data[[4]]$legendgroup <- "Signal detection period"
    plt$x$data[[5]]$name <- plt$x$data[[5]]$legendgroup <- "Threshold"
    plt$x$data[[6]]$showlegend <- FALSE

    if (padding && any(!is.na(results$expected_pad))) {
      plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Expected"
      if (!is.null(intervention_date)) {
        plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Intervention (pandemic)"
        plt$x$data[[8]]$showlegend <- TRUE
      }

      if (length(plt$x$data) == 9 && any(results$alarms == TRUE, na.rm = TRUE)) {
        if (is.null(intervention_date)) {
          plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Signal"
        } else {
          plt$x$data[[9]]$name <- plt$x$data[[9]]$legendgroup <- "Signal"
          plt$x$data[[9]]$showlegend <- TRUE
        }
      } else {
        if (any(results$alarms == TRUE, na.rm = TRUE)) {
          plt$x$data[[8]]$name <- plt$x$data[[8]]$legendgroup <- "Signal"
        }
      }
    } else {
      if (any(results$alarms == TRUE, na.rm = TRUE)) {
        plt$x$data[[7]]$name <- plt$x$data[[7]]$legendgroup <- "Signal"
      }
    }
  }
  return(plt)
}
