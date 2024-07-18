#' Not In
#'
#' Infix operator equivalent to `!(x %in% table)`.
#'
#' @inheritParams base::match
#' @return A logical vector, indicating if a match was not located
#'   for each element of x: thus the values are `TRUE` or `FALSE`
#'   and never `NA`.
#'
#' @name notin-infix
#' @seealso [base::match()]
#' @keywords internal
"%notin%" <- function(x, table) {
  match(x, table, nomatch = 0L) < 1L
}


#' First Max
#'
#' Identify the first occurence of the maximum value in a vector.
#'
#' @param x A vector.
#' @return A logical vector of same length as `x`.
#'
#' @keywords internal
first_max = function(x) {
  seq_along(x) == which.max(x)
}
