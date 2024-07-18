#' String Left/Right Extraction
#'
#' Extract The leftmost/rightmost characters from a string.
#'
#' @inheritParams stringr::str_trunc
#' @return A left- or right-truncated string.
#'
#' @name string-truncation
#' @importFrom stringr str_trunc
#' @keywords internal
left = function(string, width) {
  str_trunc(string, width, side = "right", ellipsis = "")
}


#' @rdname string-truncation
#' @importFrom stringr str_trunc
#' @keywords internal
right = function(string, width) {
  str_trunc(string, width, side = "left", ellipsis = "")
}


#' Extract Text Between
#'
#' Extract text between two strings.
#'
#' @param string A character vector.
#' @param before A regex string preceding the text to extract.
#'   capture groups `()` are not currently supported.
#' @param after A regex string following the text to extract.
#'   capture groups `()` are not currently supported.
#' @return A character vector of extracted strings. Unmatched strings
#'   are returned unchanged.
#'
#' @importFrom stringr str_replace str_glue str_detect
#' @keywords internal
extract_between = function(string, before, after) {
  if (any(str_detect(c(before, after), "\\(|\\)"))) {
    warning("'(' or ')' detected in argument \"before\" or \"after\",",
    " may not return expected results.")
  }
  # before and after are wrapped in (), text to extract is second group
  str_replace(string, str_glue("({before})\\s*(.*?)\\s*({after})"),
    "\\2")
}
