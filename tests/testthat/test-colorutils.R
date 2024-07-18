test_that("color to RGBA works", {

  colors = c("red", "red", "white", "blue")
  alphas = c(1, 0, 0.5, 0)
  colmat = matrix(
    c(
      255, 0, 0, 1,
      255, 0, 0, 0,
      255, 255, 255, 0.5,
      0, 0, 255, 0
    ), ncol = 4,
    dimnames = list(c("red", "green", "blue", "alpha"), NULL)
  )

  # need to use cbind() to match output when selecting columns with []
  expect_identical(color_to_rgba(colors[1]), cbind(colmat[, 1]))
  expect_identical(color_to_rgba("red", 0), cbind(colmat[, 2]))
  expect_identical(color_to_rgba("white", 0.5), cbind(colmat[, 3]))
  expect_identical(color_to_rgba(c("red", "white"), c(1, 0.5)),
    colmat[, c(1, 3)])
  # alpha vector recycling
  expect_identical(color_to_rgba(c("red", "blue", "red"), c(1, 0)),
    colmat[, c(1, 4, 1)])

})

test_that("RGBA to hexadecimal works", {

  colmat = matrix(
    c(
      255, 0, 0, 1,
      255, 0, 0, 0,
      255, 255, 255, 0.5,
      0, 0, 255, 0
    ), ncol = 4,
    dimnames = list(c("red", "green", "blue", "alpha"), NULL)
  )
  hexes = c("#FF0000FF", "#FF000000", "#FFFFFF80", "#0000FF00")

  expect_identical(rgba_to_hex(colmat[, 1]), hexes[1])
  expect_identical(rgba_to_hex(colmat[, c(1, 3)]), hexes[c(1, 3)])
  expect_identical(rgba_to_hex(colmat), hexes)

})

test_that("color to hexadecimal works", {

  colors = c("red", "red", "white", "blue")
  alphas = c(1, 0, 0.5, 0)
  hexes = c("#FF0000FF", "#FF000000", "#FFFFFF80", "#0000FF00")

  expect_identical(color_to_hex(colors[1], 1), hexes[1])
  expect_identical(color_to_hex(colors, alphas), hexes)
  # alpha vector recycling
  expect_identical(color_to_hex(c("red", "blue", "red"), c(1, 0)),
    hexes[c(1, 4, 1)])


})
