test_that("String manipulation works", {

  strings1 = c("foobarbaz", "fooba", "foo", "")
  expect_identical(left(strings1, 5), c("fooba", "fooba", "foo", ""))
  expect_identical(left(NULL, 5), NULL)
  expect_identical(right(strings1, 5), c("arbaz", "fooba", "foo", ""))
  expect_identical(right(NULL, 5), NULL)

  strings2 = c("foobarbaz", "foo/gar/baz", "fuu/bar/guu", "fuhbuhguh")
  expect_identical(extract_between(strings2, "foo", "baz"),
    c("bar", "/gar/", strings2[3:4]))
  expect_identical(extract_between(strings2, before = "foo|fuu|fuh",
    after =  "baz|guu|guh"), c("bar", "/gar/", "/bar/", "buh"))
  expect_warning(extract_between(strings2, "(foo|fuu|fuh)", "baz"))

})
