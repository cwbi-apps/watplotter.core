test_that("wateryear works", {
  # sample data
  dt2002 = seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
    as.POSIXct("2002-09-30 12:00:00", tz = "UTC"), by = "1 day")
  dt2003 = seq(as.POSIXct("2002-10-01 12:00:00", tz = "UTC"),
    as.POSIXct("2002-12-31 12:00:00", tz = "UTC"), by = "1 day")
  x = c(dt2002, dt2003)
  wy2002 = rep(2002L, length(dt2002))
  wy2003 = rep(2003L, length(dt2003))
  ry2002 = rep(3001L, length(dt2002))
  ry2003 = rep(3000L, length(dt2003))
  wyx = wateryear(x)
  wyxd = reference_date(x)
  # basics
  expect_vector(wyx, integer(0), length(x))
  expect_vector(wyxd, as.POSIXct(character(0), "UTC"), length(x))
  # water year assigned correctly
  expect_identical(
    wyx,
    c(wy2002, wy2003)
  )

  # reference datetime assigned correctly
  # strftime() does not use input timezone by default, so use format()
  expect_identical(
    format(wyxd, "%Y-%m-%d %H:%M:%S"),
    paste(c(ry2002, ry2003), format(x, "%m-%d %H:%M:%S"), sep = "-")
  )
  # timezone is preserved
  expect_identical(
    attr(x, "tzone"),
    attr(wyxd, "tzone")
  )

  # reference year retrieved correctly
  expect_identical(
    get_reference_year(wyxd),
    rep(3001L, length(wyxd))
  )

})

test_that("end of month works", {
  # sample data  
  x1 = as.POSIXct(sprintf("2003-%02d-05 05:44:23", 1:12),
    tz = "US/Pacific")
  x2 = as.POSIXct(sprintf("2004-%02d-05 05:44:23", 1:12),
    tz = "US/Pacific")
  eom1 = as.POSIXct(sprintf("2003-%02d-%02d 23:59:59", 1:12,
    c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    tz = "US/Pacific")
  eom2 = as.POSIXct(sprintf("2004-%02d-%02d 05:44:23", 1:12,
    c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    tz = "US/Pacific")

  expect_equal(end_of_month(x1), eom1)
  expect_equal(end_of_month(x2, TRUE), eom2)

})


test_that("end of month works", {
  # sample data  
  x1 = as.POSIXct(sprintf("2003-%02d-05 05:44:23", 1:12),
    tz = "US/Pacific")
  x2 = as.POSIXct(sprintf("2004-%02d-05 05:44:23", 1:12),
    tz = "US/Pacific")
  eom1 = as.POSIXct(sprintf("2003-%02d-%02d 23:59:59", 1:12,
    c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    tz = "US/Pacific")
  eom2 = as.POSIXct(sprintf("2004-%02d-%02d 05:44:23", 1:12,
    c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
    tz = "US/Pacific")

  expect_equal(end_of_month(x1), eom1)
  expect_equal(end_of_month(x2, TRUE), eom2)

})


test_that("window to range works", {

  expect_error(window_to_range("foo"))
  expect_error(window_to_range("feb32"))
  expect_error(window_to_range("32feb"))
  expect_error(window_to_range("feb", reference_year = "foo"))

  expect_equal(
    window_to_range("feb"),
    lubridate::ymd_hm(c("3001-02-01 00:00", "3001-02-28 24:00"))
  )
  expect_equal(
    window_to_range("feb", 2020L),
    lubridate::ymd_hm(c("2020-02-01 00:00", "2020-02-29 24:00"))
  )
  expect_equal(
    window_to_range("01feb"),
    lubridate::ymd_hm(c("3001-02-01 00:00", "3001-02-01 24:00"))
  )
  expect_equal(
    window_to_range("apr-may"),
    lubridate::ymd_hm(c("3001-04-01 00:00", "3001-05-31 24:00"))
  )
  expect_equal(
    window_to_range("may-apr"),
    lubridate::ymd_hm(c("3000-05-01 00:00", "3001-04-30 24:00"))
  )
  expect_equal(
    window_to_range("20may-apr"),
    lubridate::ymd_hm(c("3000-05-20 00:00", "3001-04-30 24:00"))
  )
  expect_equal(
    window_to_range("may-15apr"),
    lubridate::ymd_hm(c("3000-05-01 00:00", "3001-04-15 24:00"))
  )
  expect_equal(
    window_to_range("20may-15apr"),
    lubridate::ymd_hm(c("3000-05-20 00:00", "3001-04-15 24:00"))
  )
  # test explicit time stamps
  expect_equal(
    window_to_range("20may 01:00-15apr 17:58"),
    lubridate::ymd_hm(c("3000-05-20 01:00", "3001-04-15 17:58"))
  )
  expect_equal(
    window_to_range("20may 01:00-15apr"),
    lubridate::ymd_hm(c("3000-05-20 01:00", "3001-04-15 24:00"))
  )
  expect_equal(
    window_to_range("20may-15apr 17:00"),
    lubridate::ymd_hm(c("3000-05-20 00:00", "3001-04-15 17:00"))
  )

})
