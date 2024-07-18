test_that("field assertion works", {

  df1 = data.frame(a = 1, b = 2, c = 3)

  expect_error(assert_fields(df1))
  expect_identical(assert_fields(df1, "a"), NULL)
  expect_identical(assert_fields(df1, "b"), NULL)
  expect_identical(assert_fields(df1, c("a", "b")), NULL)
  expect_error(assert_fields(df1, c("a", "b", "C")))
  expect_identical(assert_fields(df1, c("a", "b", "c")), NULL)
  expect_error(assert_fields(df1, NA))
  expect_identical(assert_fields(df1, NULL), NULL)

})
