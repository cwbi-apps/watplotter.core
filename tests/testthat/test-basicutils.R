test_that("infixes work", {
 
  expect_identical("" %notin% LETTERS, TRUE)
  expect_identical(LETTERS[1] %notin% "", TRUE)

  expect_identical(letters[1:3] %notin% LETTERS, rep(TRUE, 3))
  expect_identical(LETTERS[1:3] %notin% LETTERS, rep(FALSE, 3))
  expect_identical(c("a", "B", "c") %notin% LETTERS, c(TRUE, FALSE, TRUE))

  expect_identical(LETTERS[1] %notin% NULL, TRUE)
  expect_identical(LETTERS[1] %notin% character(0), TRUE)
  expect_identical(NULL %notin% LETTERS, logical(0))
  expect_identical(character(0) %notin% LETTERS, logical(0))

})
