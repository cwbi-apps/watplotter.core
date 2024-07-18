#' Classify ICF Dates
#'
#' Classify an ICF Dates.
#'
#' @param df A numeric vector, typically the numeric representation of
#'   the ICF date.
#' @return The dataset, with additional field "icf".
#'
#' @export
classify_icf = function(df) {
  assert_fields(df, fields("value"))

  mod = mutate(df, icf = factor(.data[[fields("value")]] > 0,
      c(FALSE, TRUE), c("pre-ICF", "post-ICF")))
  if (anyNA(mod$icf)) {
    warning("ICF dates contain one or more NA values.")
  }
  rename_fields(mod, "icf", fields("icf"))
}


#' Assign ICF Periods
#'
#' Label dates based on an existing ICF classification.
#'
#' @inheritParams rename_fields
#' @param icf_periods A data frame of ICF classes. At minimum, the
#'   fields "event", "date", and "icf" are required. Additional
#'   fields "alt" and "run" are also used to assign categories to
#'   `df` if they are present in `icf_periods`.
#'
#' @inheritParams assign_probabilities
#' @return The dataset, with additional field "icf".
#'
#' @importFrom dplyr left_join all_of select
#' @export
assign_icf = function(df, icf_periods) {
  assert_fields(df, fields("event", "date"))
  assert_fields(icf_periods, fields("event", "date", "icf"))

  if (fields("icf") %in% names(df)) {
    stop(sprintf("Argument \"df\" already contains field \"%s\".",
        fields("icf")))
  }

  join_fields = intersect(fields("alt", "run", "event", "date"),
    intersect(names(df), names(icf_periods)))

  mod = left_join(df,
    select(icf_periods, all_of(c(join_fields, fields("icf")))),
    by = join_fields)

  if (anyNA(pull(mod, fields("icf")))) {
    warning("Some entries in \"df\" were not assigned ICF periods.")
  }

  mod
}


#' Extract ICF Period
#'
#' Extract ICF Periods from a dataset.
#'
#' @inheritParams rename_fields
#' @param icf_period A vector of ICF period labels.
#' @return A subset of `df`.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#' @export
extract_icf = function(df, icf_period) {
  assert_fields(df, fields("icf"))
  filter(df, .data[[fields("icf")]] %in% .env$icf_period)
}
