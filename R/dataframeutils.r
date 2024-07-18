#' Standardize Dataset
#'
#' Standardize a WAT Plotter dataset as follows:
#'
#' 1. Assert required fields are present.
#' 2. Add water year field (integer).
#' 3. Add reference date field (Date).
#'
#' timestamps in a dataframe to a reference water year.
#'
#' @inheritParams rename_fields
#' @inheritParams wateryear
#' @inheritParams reference_date
#' @param rm.leap If `TRUE`, remove leap days.
#' @return The dataframe, with new fields.
#'
#' @details Note that leap days will be converted to `NA` if
#'   `reference_year` is not a leap year.
#'
#' @importFrom lubridate leap_year yday
#' @importFrom dplyr filter near
#' @importFrom rlang .data
#' @export
standardize_dataset = function(df, rm.leap = TRUE, reference_year = 3001L) {
  assert_fields(df)
  if (rm.leap) {
    df = filter(df, !(leap_year(.data[[fields("date")]]) &
      near(yday(.data[[fields("date")]]), 60)))
  }
  df[fields("refdate")] = reference_date(df[[fields("date")]],
    reference_year)
  df[fields("wateryear")] = wateryear(df[[fields("date")]])
  df
}


#' Classify Water Years
#'
#' Label water years based on cutoffs.
#'
#' @inheritParams rename_fields
#' @param cutoffs A named vector of cutoffs defining
#'   the upper ends of the water year categories. The cutoffs and labels
#'   are passed to [base::cut()], with `Inf` appended to cutoffs to
#'   complete the definition of the highest class. The lowest class
#'   cutoff should typically be specified as `0` or `-Inf` to avoid
#'   `NA` values being returned.
#' @inheritParams assign_probabilities
#' @return The dataset, with additional field "wycategory".
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data .env
#' @export
classify_wateryear = function(df, cutoffs) {
  assert_fields(df, fields("value"))
  cutoffs = sort(cutoffs)
  mod = mutate(df,
    wycategory = cut(.data[[fields("value")]],
      breaks = c(unname(.env$cutoffs), Inf),
      labels = names(.env$cutoffs),
      right = FALSE, include.lowest = TRUE))
  if (anyNA(mod$wycategory)) {
    warning("Specified \"cutoffs\" returned one or more NA values.")
  }
  rename_fields(mod, "wycategory", fields("wycategory"))
}


#' Assign Water Year Category
#'
#' Label water years based on an existing classification.
#'
#' @inheritParams rename_fields
#' @param wy_categories A data frame of categories. At minimum, the fields
#'   "wateryear" and "wycategory" are required. Additional fields
#'   "alt", "run" and "event" are also used to assign categories to
#'   `df` if they are present in `wy_categories`.
#'
#' @inheritParams assign_probabilities
#' @return The dataset, with additional field "wycategory".
#'
#' @importFrom dplyr distinct left_join all_of pull count
#' @export
assign_wycategory = function(df, wy_categories) {
  assert_fields(df, fields("wateryear"))
  assert_fields(wy_categories, fields("wateryear", "wycategory"))

  if (fields("wycategory") %in% names(df)) {
    stop(sprintf("Argument \"df\" already contains field \"%s\".",
        fields("wycategory")))
  }

  join_fields = intersect(fields("alt", "run", "event", "wateryear"),
    intersect(names(df), names(wy_categories)))

  wyclasses = distinct(wy_categories,
    across(all_of(c(join_fields, fields("wycategory")))))

  class_count = count(wyclasses, across(all_of(join_fields)),
    name = "num_entries")

  if (any(pull(class_count, "num_entries") > 1L)) {
    stop(
      sprintf("Multiple '%s' entries for each (%s) group.",
        fields("wycategory"), paste(join_fields, collapse = ", "))
    )
  }

  mod = left_join(df, wyclasses, by = join_fields)

  if (anyNA(pull(mod, fields("wycategory")))) {
    warning("Some entries in \"df\" were not assigned ",
      "water year categories.")
  }

  mod
}


#' Extract Water Year Category
#'
#' Extract water year categories from a dataset.
#'
#' @inheritParams rename_fields
#' @param wy_category A vector of water year categories.
#' @return A subset of `df`.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#' @export
extract_wycategory = function(df, wy_category) {
  assert_fields(df, fields("wycategory"))
  filter(df, .data[[fields("wycategory")]] %in% .env$wy_category)
}


#' Rename Fields
#'
#' Rename fields in a data frame.
#'
#' @param df A dataframe.
#' @param old_names A vector of names that may be in `df`.
#' @param new_names A vector of names to replace those in `old_names`.
#' @return The dataframe, potentially with new fields names.
#'
#' @importFrom stringr str_replace_all str_glue
#' @importFrom purrr map set_names
#' @export
rename_fields = function(df, old_names, new_names) {
  stopifnot(length(old_names) == length(new_names))
  set_names(df, str_replace_all(names(df),
    set_names(new_names, str_glue("^{old_names}$"))))
}


#' Extract Window
#'
#' Extract time windows from a dataset.
#'
#' @inheritParams standardize_dataset
#' @inheritParams window_to_range
#' @return A subset of `df` containing the specified time window for
#'   from each year, with additional "period" field.
#'
#' @details Note that leap days will be implicitly removed when the
#'   reference year is not a leap year, even if `rm.leap = FALSE`. In
#'   the edge case of `time_window = "feb-jan"`, leap days are
#'   removed according to whether the year preceding `reference_year`
#'   is a leap year, since February will be associated with the
#'   preceding year rather than the reference year.
#'
#' @importFrom dplyr filter between bind_rows
#' @importFrom rlang .data .env
#' @importFrom purrr map
#' @export
extract_window = function(df, time_window) {
  assert_fields(df, fields("refdate"))
  # convert window to range using reference year
  ry = get_reference_year(max(df[[fields("refdate")]]))
  time_ranges = map(set_names(time_window), function(tw)
    window_to_range(tw, ry))

  mod = bind_rows(map(time_ranges, function(tr)
      filter(df, between(.data[[fields("refdate")]], tr[1], tr[2]))),
    .id = "period")

  rename_fields(mod, "period", fields("period"))
}


#' Extract Base Years
#'
#' Extract base years from a dataset.
#'
#' @inheritParams rename_fields
#' @param base_year An integer vector of base years.
#' @return A subset of `df`.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#' @export
extract_baseyear = function(df, base_year) {
  assert_fields(df, fields("baseyear"))
  filter(df, .data[[fields("baseyear")]] %in% .env$base_year)
}


#' Add Base Years
#'
#' Add base year information to a dataset.
#'
#' @inheritParams rename_fields
#' @param event_years A dataframe containing at minimum the fields
#'   "event" and "baseyear". Fields "alt", "run", and
#'   "wateryear" will also be used to attach baseyear information if
#'   available.
#' @return The dataframe `df`, with additional fields "baseyear".
#'
#' @importFrom dplyr left_join across all_of
#' @export
assign_baseyear = function(df, event_years) {
  assert_fields(df, "event")
  assert_fields(event_years, fields("event", "baseyear"))
  join_fields = intersect(fields("alt", "run", "event", "wateryear"),
    intersect(names(df), names(event_years)))

  mod = left_join(df,
    distinct(event_years,
      across(all_of(c(join_fields, fields("baseyear"))))),
    by = join_fields)

  if (anyNA(pull(mod, fields("baseyear")))) {
    warning("Some entries in \"df\" were not assigned ",
      "base years.")
  }

  mod
}

#' Extract Peaks
#'
#' Extract the peak values from a dataset.
#'
#' @inheritParams rename_fields
#' @param by The field to group by. Default is `"event"`.
#' @return A subset of `df` containing peak records.
#'
#' @importFrom dplyr group_by filter ungroup across all_of
#' @importFrom rlang .data
#' @export
extract_peaks = function(df, by = fields("event")) {
  assert_fields(df, c(by, fields("value")))
  ungroup(filter(group_by(df, across(all_of(by))),
    first_max(.data[[fields("value")]])))
}


#' Assign Probabilities
#'
#' Assign incremental probabilities to events.
#'
#' @inheritParams standardize_dataset
#' @param event_weights A dataframe with fields "event" and "wypercentile".
#'   If present, fields "alt" and "run" will also be used when joining
#'   probabilities. If `NULL`, equal weight is assigned to each event.
#' @param cumulative If `TRUE`, return cumulative probabilities.
#'
#' @importFrom dplyr left_join mutate distinct across group_by ungroup
#'   arrange all_of any_of n pull
#' @importFrom rlang .data
#' @keywords internal
assign_probabilities = function(df, event_weights, cumulative = TRUE) {

  stop("`assign_probabilities()` not implemented")

  assert_fields(df, fields("event"))

  # assign equal weights if not provided
  if (missing(event_weights)) {
    event_weights = mutate(
      distinct(df, across(any_of(fields("alt", "run", "event")))),
      wypercentile = 1
    )
    event_weights = rename_fields(event_weights, "wypercentile",
      fields("wypercentile"))
  } else {
    assert_fields(event_weights, fields("event", "wypercentile"))
  }

  # rescale event weights to sum to 1
  new_weights = pull(ungroup(mutate(
    group_by(event_weights, across(any_of(fields("alt", "run")))),
    newpercentile = .data[[fields("wypercentile")]] /
      sum(.data[[fields("wypercentile")]]))), "newpercentile")
  event_weights[fields("wypercentile")] = new_weights

  if (cumulative) {
    # accumulate wypercentile by event order
    new_weights = pull(ungroup(mutate(
      arrange(
        group_by(event_weights, across(any_of(fields("alt", "run")))),
        fields("event")
      ),
      newpercentile = cumsum(.data[[fields("wypercentile")]])
    )), "newpercentile")
    event_weights[fields("wypercentile")] = new_weights
  }
  # get join fields
  join_fields = intersect(fields("alt", "run", "event"),
    intersect(names(df), names(event_weights)))
  left_join(df,
    distinct(event_weights,
      across(all_of(c(join_fields, fields("wypercentile"))))),
    by = join_fields)
}


#' Rolling Window
#'
#' Apply a rolling window function to a dataset.
#'
#' @inheritParams standardize_dataset
#' @param window_size The rolling window size. Assumes a centered
#'   rolling window.
#' @param fun The function to apply to the rolling window.
#' @param ... Additional parameters to pass to `fun`.
#' @param complete If `TRUE`, evaluate `fun` on complete windows only.
#'   Incomplete sections will be filled with `NA`.
#' @importFrom dplyr mutate group_by ungroup across any_of matches
#' @importFrom slider slide_dbl
#' @importFrom rlang .env
#' @export
rolling_window = function(df, window_size, fun = mean, ..., complete = TRUE) {
  before = floor((window_size - 1) / 2)
  after = ceiling((window_size - 1) / 2)

  roll_fun = function(x) fun(x, ...)

  ungroup(mutate(
      group_by(df, across(any_of(fields("alt", "run", "path")))),
      across(matches(fields("value")), function(x) 
        slide_dbl(x, .env$roll_fun, .before = .env$before,
          .after = .env$after, .complete = .env$complete))
  ))
}
