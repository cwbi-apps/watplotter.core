#' Water Year
#'
#' Identify the water year of the provided datetimes.
#'
#' @param datetime A vector of dates or datetimes.
#' @return A vector of integer water years.
#'
#' @importFrom lubridate month year tz
#' @keywords internal
wateryear = function(datetime, reference_year = 3001L) {
  ltime = as.POSIXlt(datetime, tz = tz(datetime))
  as.integer(year(ltime)) + as.integer(month(ltime) > 9)
}


#' Reference Date
#'
#' Return datetimes with a fixed water year. Useful for overplotting
#'   timeseries.
#'
#' @inheritParams wateryear
#' @param reference_year The reference year to use when returning
#'   datetimes. Ignored when `as_date = FALSE`.
#' @return A vector of `POSIXct` reference timestamps
#'
#' @importFrom lubridate month year<- tz
#' @keywords internal
reference_date = function(datetime, reference_year = 3001L) {
  ltime = as.POSIXlt(datetime, tz = tz(datetime))
  `year<-`(datetime, reference_year - as.integer(month(ltime) > 9))
}


#' Reference Year
#'
#' Get the reference water year from a timestamp.
#'
#' @param reftime A vector of reference datetimes, e.g., output of
#'   [reference_date()].
#' @return A vector of integer reference years.
#'
#' @importFrom lubridate month year tz
#' @keywords internal
get_reference_year = function(reftime) {
  ltime = as.POSIXlt(reftime, tz = tz(reftime))
  as.integer(year(ltime)) + as.integer(month(ltime) > 9)
}


#' End Of Month
#'
#' Retrieve end of month datetimes for a set of datetimes.
#'
#' @param datetime A vector of datetimes.
#' @param keep_times If `TRUE`, keep the HMS component of the input
#'   datetimes. If `FALSE`, return "23:59:59" as the HMS component.
#' @return A datetime vector
#' @importFrom lubridate as_datetime make_datetime ceiling_date
#'   as.duration year month day hour minute second tz
#' @keywords internal
end_of_month = function(datetime, keep_times = FALSE) {
  new_datetime = ceiling_date(datetime, "month", TRUE) -
    as.duration("1 sec")

  if (keep_times) {
    ltime = as.POSIXlt(datetime, tz = tz(datetime))
    new_ltime = as.POSIXlt(new_datetime, tz = tz(new_datetime))
    make_datetime(year(new_ltime), month(new_ltime), day(new_ltime),
      hour(ltime), minute(ltime), second(ltime), tz = tz(datetime))
  } else {
    new_datetime
  }
}


#' Window to Range
#'
#' Convert a time window to a date range using a reference year.
#'
#' @param time_window A time window specification. Can be one of
#'   `"annual"`, `"spring"`, `"winter"`, or a user-defined window
#'   in the format `"jan"`, `"jan-mar"`, `"01jan-31mar"`, etc.
#'   Time stamps such as `"30Apr 00:01"` or `"01Apr 00:01-30Apr 24:00"`
#'   are also supported. If times are ommitted the window is assumed to
#'   be inclusive, i.e., `"01jan-31mar"` will be interpreted as
#'   `"01jan 00:00-31mar 24:00"`.
#' @inheritParams wateryear
#' @return A two-element date vector.
#'
#' @importFrom stringr str_detect str_split str_extract
#' @importFrom lubridate ym ymd_hm days_in_month
#' @importFrom dplyr if_else
#' @keywords internal
window_to_range = function(time_window, reference_year = 3001L) {

  reference_year = suppressWarnings(as.integer(reference_year))
  if (!all(is.finite(reference_year - c(0, 1)))) {
    stop("Argument \"reference_year\" is not valid.")
  }
  time_window = tolower(time_window[1])
  if (time_window %in% c("annual", "spring", "winter")) {
    time_window = switch(time_window,
      "annual" = "01oct-30sep",
      "winter" = "01nov-31mar",
      "spring" = "01apr-31aug",
      time_window
    )
  }
  date_format = "[0-9]*[a-z]+( [0-9]{2}:[0-9]{2})*"
  if (!str_detect(time_window, sprintf("^%s-*%s$", date_format, date_format))) {
    stop("Argument \"time_window\" not recognized as a time range.")
  }
  # get range components, paste to ensure length-2 vector is returned
  range_ends = paste0(str_split(time_window, "-")[[1]], c("", ""))
  # months
  month_range = match(str_extract(range_ends, "[a-z]{3}"),
    tolower(month.abb))
  if (any(is.na(month_range))) {
    stop("Argument \"time_window\" not recognized as a time range.")
  }
  # years
  if (month_range[2] < month_range[1]) {
    year_range = sprintf("%04d", c(reference_year - 1L, reference_year))
  } else {
    year_range = sprintf("%04d", rep(reference_year, 2))
  }
  # days
  day_range = as.integer(str_extract(range_ends, "[0-9]*"))
  # if days not specified, assume start/end of month
  default_days = c(1, days_in_month(ym(paste0(year_range[2], "-",
    month_range[2]))))
  day_range[is.na(day_range)] = default_days[is.na(day_range)]
  time_range = str_extract(range_ends, "[0-9]{2}:[0-9]{2}")
  time_range = if_else(is.na(time_range), c("00:00", "24:00"), time_range)
  # return date range using reference year
  date_range = ymd_hm(paste(year_range, month_range, day_range, time_range),
    tz = "UTC", quiet = TRUE)
  if (any(is.na(date_range))) {
    stop("Argument \"time_window\" not recognized as a time range.")
  } else {
    date_range
  }
}
