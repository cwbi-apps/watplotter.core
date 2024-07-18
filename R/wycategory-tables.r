#' Cutoffs Store
#'
#' Session storage for standard cutoff tables.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom utils packageName
#' @keywords internal
cutoffs_store = function() {
  .store = NULL

  function() {
    if (is.null(.store)) {
      tbl_dir = system.file("standard", "wateryearcategories",
        package = packageName())
      .store <<- bind_rows(map(list.files(tbl_dir, full.names = TRUE),
        read_standard_table))
    }
    .store
  }
}
.cutoffs = cutoffs_store()


#' Read Standard Table
#'
#' Read a standard table YAML file.
#'
#' @importFrom yaml read_yaml
#' @importFrom dplyr tibble
#' @keywords internal
read_standard_table = function(filepath) {

  yaml = read_yaml(filepath)
  yaml[["cutoffs"]] = list(unlist(yaml[["cutoffs"]]))
  do.call(tibble, yaml)

}


#' Standard Water Year Category Tables
#'
#' List the standard water year category tables.
#'
#' @param name A table name to search for.
#' @param project A project name to search for.
#' @param interval An interval to search for.
#' @return A tibble.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#' @export
list_standard_cutoffs = function(name = NULL, project = NULL,
  interval = NULL) {

  tbls = .cutoffs()

  if (!is.null(name)) {
    tbls = filter(tbls, .data$name %in% .env$name)
  }
  if (!is.null(project)) {
    tbls = filter(tbls, .data$project %in% .env$project)
  }
  if (!is.null(interval)) {
    tbls = filter(tbls, .data$interval %in% .env$interval)
  }
  tbls

}


#' Get Standard Water Year Cutoffs
#'
#' Get the cutoffs from a standard water year category table.
#'
#' @param name A table name to search for.
#' @return A named numeric vector of cutoffs, with additional
#'   attributes `"project"`, `"interval"`, and  `"date"`.
#'
#' @export
standard_cutoffs = function(name) {
  tbl = list_standard_cutoffs(name)

  if (nrow(tbl) > 1L) {
    stop("Multiple standard tables returned for name: ", name)
  } else if (nrow(tbl) < 1L) {
    stop("No  standard tables matching name: ", name)
  }
  cutoffs = tbl$cutoffs[[1]]
  attr(cutoffs, "project") = tbl$project
  attr(cutoffs, "interval") = tbl$interval
  attr(cutoffs, "date") = tbl$date

  cutoffs
}
