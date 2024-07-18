#' Wat Plotter Fields
#'
#' Standard Fields in WAT Plotter data frames.
#'
#' @param ... A vector of field codes, any of: "alt", "run", "path",
#'   "event", "date", "value", "refdate", "wateryear", "wycategory",
#'   "wypercentile", "baseyear".
#' @return A vector of field names.
#'
#' @export
fields = function(...) {
  all_fields = c(
    alt = "alt",
    run = "run",
    path = "path",
    event = "event",
    period = "period",
    date = "date",
    value = "value",
    refdate = "refdate",
    wateryear = "wateryear",
    wycategory = "wycategory",
    wypercentile = "wypercentile",
    baseyear = "baseyear",
    icf = "icf"
  )
  selected_fields = c(...)
  bad_fields = setdiff(selected_fields, names(all_fields))
  if (length(bad_fields) > 0L) {
    stop("Unknown field codes: ", paste(shQuote(bad_fields),
      collapse = ", "))
  } else if (length(selected_fields) == 0L) {
    all_fields
  } else {
    unname(all_fields[selected_fields])
  }
}


#' Assert Fields
#'
#' Assert that required fields exist in data.
#'
#' @param df A data frame.
#' @param required_fields A character vector of required fields. If
#'   missing, will default to
#'   `fields(c("path", "event", "date", "value"))`.
#'
#' @export
assert_fields = function(df, required_fields) {
  if (missing(required_fields)) {
    required_fields = fields(c("path", "event", "date", "value"))
  }
  missing_fields = setdiff(required_fields,  names(df))
  if (length(missing_fields) > 0L) {
    stop("Data is missing required fields: ",
      paste(shQuote(missing_fields), collapse = ", "))
  }
  invisible()
}
