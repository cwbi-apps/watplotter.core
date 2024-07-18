#' Named Color to RGB Matrix
#'
#' Convert a vector of named colors to an RGBA matrix. This is
#'   basically a wrapper for [grDevices::col2rgb()] that supports
#'   specification of numeric `alpha` values.
#'
#' @param color A vector of named colors.
#' @param alpha A vector of alpha values. Standard vector recycling
#'   rules apply if length of `alpha` does not match `color`.
#' @return A matrix with four rows R, B, G, A and column.
#'
#' @importFrom grDevices col2rgb
#' @importFrom dplyr between
#' @keywords internal
color_to_rgba = function(color, alpha = 1) {
  stopifnot(all(between(alpha, 0, 1)))
  alpha = rep_len(alpha, length(color))
  rgbmat = col2rgb(color, TRUE)
  rgbmat[4, ] = alpha
  rgbmat
}


#' RGBA Matrix to Hexadecimal
#'
#' Convert an RGBA matrix to a vector of hexidecimal strings.
#' @param rgba A matrix with four rows for Red, Green, Blue, and
#'   Alpha channels, i.e., output of [color_to_rgba()].
#' @return A vector of hexadecimal strings.
#'
#' @importFrom grDevices rgb
#' @keywords internal
rgba_to_hex = function(rgba) {
  rgba = as.matrix(rgba, nrow = 4)
  stopifnot(nrow(rgba) == 4L)
  apply(rgba, 2, function(x)
    do.call(rgb, as.list(x / c(255, 255, 255, 1))))
}


#' Named Color to Hexadecimal
#'
#' Convert a vector of named colors to a vector of hexidecimal strings.
#'
#' @inheritParams color_to_rgba
#' @return A vector of hexadecimal strings.
#'
#' @keywords internal
color_to_hex = function(color, alpha = 1) {
  rgba_to_hex(color_to_rgba(color, alpha))
}
