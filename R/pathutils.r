#' Form Path
#'
#' Form path from parts.
#' @param A A vector of "A" path parts.
#' @param B A vector of "B" path parts.
#' @param C A vector of "C" path parts.
#' @param D A vector of "D" path parts.
#' @param E A vector of "E" path parts.
#' @param F A vector of "F" path parts.
#' @param parts A named list of path parts.
#' @return A vector of paths.
#'
#' @importFrom utils modifyList
#' @importFrom stringr str_glue_data str_detect
#' @export
form_path = function(A = "", B = "", C = "", D = "", E = "", F = "",
  parts = list()) {
  # check parts argument
  if (!all(names(parts) %in% LETTERS[1:6])) {
    bad_parts = setdiff(names(parts), LETTERS[1:6])
    stop("Unrecognized parts: ", paste(shQuote(bad_parts), collapse = ", "))
  }
  parts_list = data.frame(modifyList(
    list(A = A, B = B, C = C, D = D, E = E, F = F),
    parts
  ))
  if (!all(nzchar(paste0(parts_list[["A"]], parts_list[["B"]])))) {
    stop("'A' part or 'B' part must be specified.")
  }
  if (any(unlist(sapply(parts_list, str_detect, "/")))) {
    stop("Path parts cannot contain '/' character.")
  }
  str_glue_data(parts_list, "/{A}/{B}/{C}/{D}/{E}/{F}/")
}


#' Path Checking
#'
#' Check path format.
#'
#' @inheritParams path_to_parts
#' @return The path (invisibly).
#'
#' @importFrom stringr str_count str_detect
#' @export
check_path = function(path) {
  checks = str_detect(path, "(^/(.+/.*|.*/.+)/.*/.*/.*/.*/$)")
  if (!all(checks)) {
    stop("One or more paths are malformed:\n",
      paste(str_glue("    {which(!checks)}: {path[!checks]}"),
        collapse = "\n"),
      "\nexpected format is \"/a/b/c/d/e/f/\"")
  }
  invisible(path)
}


#' Path To Parts
#'
#' Split a vector of paths into a dataframe of parts.
#'
#' @param path A vector of paths.
#' @return A dataframe with fields "A", "B", "C", "D", "E", "F"
#'   containing the path parts.
#'
#' @importFrom purrr map set_names
#' @importFrom stringr str_split
#' @importFrom dplyr bind_rows
#' @export
path_to_parts = function(path) {
  check_path(path)
  parts = str_split(path, "/")
  bind_rows(map(parts, function(x)
    as.list(set_names(x[2:7], LETTERS[1:6]))
  ))
}


#' Get Path Part
#'
#' Extract a specific part from a path.
#'
#' @inheritParams path_to_parts
#' @param part A path part, i.e., one of `"A"`, `"B"`, `"C"`, `"D"`,
#'   `"E"`, `"F"`.
#' @return A vector of path parts.
#'
#' @importFrom dplyr pull
#' @importFrom rlang .env
#' @export
get_path_part = function(path, part = c("A", "B", "C", "D", "E", "F")) {
  part = match.arg(toupper(part), LETTERS[1:6], several.ok = FALSE)
  pull(path_to_parts(path), .env$part)
}


#' Replace Path Part
#'
#' Replace specific parts from of a path.
#'
#' @inheritParams path_to_parts
#' @param ... Path parts to replace.
#' @return A vector of paths.
#'
#' @examples
#' replace_path_part("/a/b/c/d/e/f/", A = "foo", C = "", F = ".*")
#'
#' @importFrom utils modifyList
#' @export
replace_path_part = function(path, ...) {
  form_path(parts = modifyList(path_to_parts(path), list(...)))
}


#' Drop Path Part
#'
#' Drop specific parts from of a path.
#'
#' @inheritParams get_path_part
#' @return A vector of paths.
#'
#' @examples
#' replace_path_part("/a/b/c/d/e/f/", A = "foo", C = "", F = ".*")
#'
#' @export
drop_path_part = function(path, part = c("A", "B", "C", "D", "E", "F")) {
  part = match.arg(toupper(part), LETTERS[1:6], several.ok = TRUE)
  form_path(parts = path_to_parts(path)[setdiff(LETTERS[1:6], part)])
}
