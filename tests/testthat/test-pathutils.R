test_that("path checking works", {

  expect_identical(check_path("/aaa/bbb/ccc/ddd/eee/fff/"),
    "/aaa/bbb/ccc/ddd/eee/fff/")
  expect_error(check_path("///////"))
  expect_error(check_path("//"))
  expect_error(check_path("///ccc////"))
  expect_identical(check_path("/aaa//////"), "/aaa//////")
  expect_identical(check_path("//bbb/////"), "//bbb/////")

})

test_that("Path forming works", {

  expect_error(form_path())
  expect_error(form_path("fo/oo"))
  expect_error(form_path(C = "ccc"))
  expect_identical(form_path("aaa"), "/aaa//////")
  expect_identical(form_path(B = "bbb"), "//bbb/////")
  expect_identical(form_path("aaa", "bbb", "ccc", "ddd", "eee", "fff"),
    "/aaa/bbb/ccc/ddd/eee/fff/")
  expect_identical(form_path(c("aa", "aaa"), "bbb", "ccc", "ddd",
    "eee", "fff"), c("/aa/bbb/ccc/ddd/eee/fff/",
    "/aaa/bbb/ccc/ddd/eee/fff/"))
  expect_identical(form_path(c("aa", "aaa"), "bbb", c("cc", "ccc"),
    "ddd", "eee", "fff"), c("/aa/bbb/cc/ddd/eee/fff/",
    "/aaa/bbb/ccc/ddd/eee/fff/"))
  expect_error(form_path(c("aa", "aaa"), "bbb", "ccc",
    c("dd", "ddd", "dddd"), "eee", "fff"))

})

test_that("Path splitting works", {

  testpaths = c(
    "/aa/bbb/ccc/ddd/eee/fff/",
    "/aaa/bbb/ccc/ddd/eee/fff/",
    "/aaa///ddd/eee//",
    "//bbb//ddd//fff/"
  )
  result = path_to_parts(testpaths)
  expect_identical(result$A, c("aa", "aaa", "aaa", ""))
  expect_identical(result$B, c("bbb", "bbb", "", "bbb"))
  expect_identical(result$C, c("ccc", "ccc", "", ""))
  expect_identical(result$D, c("ddd", "ddd", "ddd", "ddd"))
  expect_identical(result$E, c("eee", "eee", "eee", ""))
  expect_identical(result$F, c("fff", "fff", "", "fff"))

})

test_that("Path part extraction works", {

  testpaths = c(
    "/aa/bbb/ccc/ddd/eee/fff/",
    "/aaa/bbb/ccc/ddd/eee/fff/",
    "/aaa///ddd/eee//",
    "//bbb//ddd//fff/"
  )
  expect_error(get_path_part(testpaths, "Q"))
  expect_error(get_path_part(NULL, "A"))
  expect_identical(get_path_part(testpaths, "A"),
    c("aa", "aaa", "aaa", ""))
  expect_identical(get_path_part(testpaths, "B"),
    c("bbb", "bbb", "", "bbb"))
  expect_identical(get_path_part(testpaths, "C"),
    c("ccc", "ccc", "", ""))
  expect_identical(get_path_part(testpaths, "D"),
    c("ddd", "ddd", "ddd", "ddd"))
  expect_identical(get_path_part(testpaths, "E"),
    c("eee", "eee", "eee", ""))
  expect_identical(get_path_part(testpaths, "F"),
    c("fff", "fff", "", "fff"))

})

test_that("Path part replacement works", {

  expect_identical(
    replace_path_part("/aa/bbb/ccc/ddd/eee/fff/",
      A = "foo", B = "", F = ".*"),
    "/foo//ccc/ddd/eee/.*/"
  )
  expect_error(replace_path_part("/aa/bbb/ccc/ddd/eee/fff/", a = "f"))
})

test_that("Path part drop works", {

  expect_identical(
    drop_path_part("/aa/bbb/ccc/ddd/eee/fff/", c("A", "F")),
    "//bbb/ccc/ddd/eee//"
  )
  expect_error(drop_path_part("/aa/bbb/ccc/ddd/eee/fff/", "G"))
})
