test_that("Renaming works", {

  df1 = data.frame(a = 1, b = 2, cc = 3)

  expect_error(rename_fields(NULL, letters, LETTERS))
  expect_error(rename_fields(df1, letters, LETTERS[1:3]))
  expect_error(rename_fields(df1, letters[1:3], LETTERS))
  expect_error(rename_fields(df1, NULL, NULL))
  expect_error(rename_fields(df1, character(0), character(0)))

  expect_identical(names(rename_fields(df1, letters, LETTERS)),
    c("A", "B", "cc"))
  expect_identical(rename_fields(df1, LETTERS, letters),
    df1)
  expect_identical(names(rename_fields(df1, letters,
    rep(NA_character_, 26))), c(NA, NA, "cc"))
  expect_identical(rename_fields(df1, "", ""), df1)

})


test_that("Data standardization works", {

  df1 = data.frame(alt = NA, event = NA, path = NA,
    date = c(seq(as.POSIXct("1981-05-01 12:00:00", tz = "UTC"),
    as.POSIXct("2021-09-30 12:00:00", tz = "UTC"), length.out = 10),
      as.POSIXct("2020-02-29 14:00:00")), value = NA)

  newdf1 = standardize_dataset(df1, rm.leap = FALSE)
  newdf2 = standardize_dataset(df1, rm.leap = TRUE)
  newdf3 = standardize_dataset(df1, FALSE, 2004L)

  expect_equal(year(newdf1[[fields("refdate")]]),
    3000 + c(1, 0, 1, 0, 1, 0, 1, 0, 1, 1, NA))
  expect_equal(year(newdf2[[fields("refdate")]]),
    3000 + c(1, 0, 1, 0, 1, 0, 1, 0, 1, 1))
  expect_equal(year(newdf3[[fields("refdate")]]),
    2003 + c(1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1))
  expect_equal(newdf3[[fields("wateryear")]],
    c(1981, 1986, 1990, 1995, 1999, 2004, 2008, 2013, 2017, 2021,
      2020))

})


test_that("Water year classification works", {

  testclass1 = c(
    "very dry" = 0,
    "dry" = 10,
    "average" = 15,
    "wet" = 20,
    "very wet" = 50
  )

  testclass2 = c(
    "dry" = 0,
    "average" = 15,
    "wet" = 20
  )

  df1 = data.frame(alt = "A", path = "A",
    date = c(seq(as.POSIXct("1981-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
      length.out = 10), as.POSIXct("2020-02-29 14:00:00")),
    event = seq_len(11), value = c(1:3, 17:20, 5, 90, 13, 44))

  expect_equal(
    classify_wateryear(df1, testclass1)$wycategory,
    factor(c(rep("very dry", 3), rep("average", 3), "wet", "very dry",
        "very wet", "dry", "wet"), levels = names(testclass1))
  )

  expect_equal(
    classify_wateryear(df1, testclass2)$wycategory,
    factor(c(rep("dry", 3), rep("average", 3), "wet", "dry",
        "wet", "dry", "wet"), levels = names(testclass2))
  )

})


test_that("window extaction works", {

  df1 = standardize_dataset(data.frame(alt = NA, event = NA, path = NA,
    date = sort(c(seq(as.POSIXct("1981-05-01 12:00:00", tz = "UTC"),
      as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
    length.out = 10), as.POSIXct("2020-02-29 14:00:00"))),
    value = rnorm(11)))
  mat = expand.grid(1983:1985, 1:12, 29, "12:00:00")[-c(4L, 6L),]
  df2 = standardize_dataset(data.frame(alt = NA, event = NA, path = NA,
    date = sort(as.POSIXct(paste0(mat[, 1], "-", mat[, 2],
      "-", mat[, 3], " ", mat[, 4]), tz = "UTC")),
    value = rnorm(34)))

  expect_equal(
    extract_window(df1, "annual"),
    cbind(period = "annual", df1[1:10, ])
  )
  expect_equal(
    extract_window(df1, "spring"),
    cbind(period = "spring", df1[c(1, 3, 5, 7, 9), ]),
    ignore_attr = "row.names"
  )
  expect_equal(
    extract_window(df1, "winter"),
    cbind(period = character(0), df1[integer(0), ]),
    ignore_attr = "row.names"
  )
  expect_equal(
    extract_window(df1, "15apr-may"),
    cbind(period = "15apr-may", df1[c(1, 3, 5), ]),
    ignore_attr = "row.names"
  )
  expect_equal(
    extract_window(df2, "winter"),
    cbind(period = "winter", df2[c(1:2, 10:13, 21:24, 32:33), ]),
    ignore_attr = "row.names"
  )
  expect_equal(
    extract_window(df2, "feb-jan"),
    cbind(period = "feb-jan", df2[c(1, 9:12, 20:23, 31:33), ]),
    ignore_attr = "row.names"
  )


  expect_equal(
    extract_window(df1, c("spring", "15apr-may")),
    rbind(
      cbind(period = "spring", df1[c(1, 3, 5, 7, 9), ]),
      cbind(period = "15apr-may", df1[c(1, 3, 5), ])
    ),
    ignore_attr = "row.names"
  )

})


test_that("peak extraction works", {
  df1 = data.frame(event = rep(1:5, each = 10),
    value = seq_len(50))
  peakdf1 = data.frame(event = 1:5,
    value  = c(10, 20, 30, 40, 50))

  expect_equal(extract_peaks(df1), peakdf1, ignore_attr = "class")
  expect_error(extract_peaks(df1, "foo"))
  expect_error(extract_peaks(df1, field_value = "foo"))

})


test_that("water year category assignment works", {

  testclass1 = data.frame(alt = "A", run = "test", event = 1L,
    wateryear = c(2002L:2012L),
    wycategory = factor(rep(c("dry", "average", "wet"),
      length.out = 11L), levels = c("dry", "average", "wet")))

  df1 = standardize_dataset(data.frame(
    alt = "A", run = "test", path = "A", event = 1L,
    date = c(seq(as.POSIXct("2002-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2011-09-30 12:00:00", tz = "UTC"),
      length.out = 10), as.POSIXct("2012-02-28 14:00:00", tz = "UTC")),
    value = c(1:3, 17:20, 5, 90, 13, 44)
  ))

  expect_identical(
    assign_wycategory(df1, testclass1)$wycategory,
    testclass1$wycategory
  )


  testclass2 = data.frame(
    event = rep(1L:4L, each = 3L),
    wateryear = rep(c(2002L:2004L), 4L),
    wycategory = factor(
      c(
        "wet", "wet", "dry",
        "wet", "average", "average",
        "average", "average", "dry",
        "wet", "average", "dry"
      ), levels = c("dry", "average", "wet")
    )
  )

  df2 = standardize_dataset(rbind(
    data.frame(
      alt = "A", run = "test", path = NA, event = 1L,
      date = seq(as.POSIXct("2002-05-01 12:00:00", tz = "UTC"),
        as.POSIXct("2004-09-30 12:00:00", tz = "UTC"),
        length.out = 3),
      value = NA
    ),
    data.frame(
      alt = "B", run = "test", path = NA, event = 3L,
      date = seq(as.POSIXct("2002-05-01 12:00:00", tz = "UTC"),
        as.POSIXct("2003-09-30 12:00:00", tz = "UTC"),
        length.out = 3),
      value = NA
    )
  ))

  expect_identical(
    assign_wycategory(df2, testclass2)$wycategory,
    factor(c("wet", "wet", "dry", "average", "average", "average"),
      levels = c("dry", "average", "wet"))
  )

  expect_warning(expect_identical(
    assign_wycategory(df2, testclass1)$wycategory,
    factor(c("dry", "average", "wet", rep(NA_character_, 3)),
      levels = c("dry", "average", "wet"))
  ))

  expect_error(
    assign_wycategory(cbind(df2, wycategory = NA), testclass2)
  )

})


test_that("water year class extraction works", {

  df1 = standardize_dataset(data.frame(alt = NA, event = NA, path = NA,
    date = sort(c(seq(as.POSIXct("1981-05-01 12:00:00", tz = "UTC"),
      as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
    length.out = 10), as.POSIXct("2020-02-29 14:00:00"))),
    value = rnorm(11),
    wycategory = factor(rep(c("dry", "average", "wet"), c(3, 3, 5)),
      c("dry", "average", "wet"))
  ))

  expect_identical(
    extract_wycategory(df1, "average"),
    df1[4:6, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    extract_wycategory(df1, c("dry", "wet")),
    df1[c(1:3, 7:10), ],
    ignore_attr = "row.names"
  )

})


test_that("assign_baseyear works", {

  df1 = standardize_dataset(data.frame(
    alt = "A", run = "test", path = "A",
    event = seq_len(11L),
    date = c(seq(as.POSIXct("2002-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2011-09-30 12:00:00", tz = "UTC"),
      length.out = 10), as.POSIXct("2012-02-28 14:00:00", tz = "UTC")),
    value = c(1:3, 17:20, 5, 90, 13, 44)
  ))
  by1 = data.frame(
    baseyear = factor(seq(3002L, 3012L)),
    event = seq_len(11L)
  )

  df2 = standardize_dataset(data.frame(
    alt = "A", run = "test", path = "A",
    event = 11L,
    date = c(seq(as.POSIXct("2002-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2011-09-30 12:00:00", tz = "UTC"),
      length.out = 10), as.POSIXct("2012-02-28 14:00:00", tz = "UTC")),
    value = c(1:3, 17:20, 5, 90, 13, 44)
  ))
  by2 = data.frame(
    wateryear = seq(2002L, 2012L),
    baseyear = factor(seq(3002L, 3012L)),
    event = 11L
  )

  expect_identical(
    assign_baseyear(df1, by1),
    cbind(df1, by1[c("baseyear")])
  )

  expect_identical(
    assign_baseyear(df2, by2),
    cbind(df2, by2[c("baseyear")])
  )


})


test_that("extract_baseyear works", {

  df1 = standardize_dataset(data.frame(
    alt = "A", run = "test", path = "A",
    event = seq_len(11L),
    date = c(seq(as.POSIXct("2002-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2011-09-30 12:00:00", tz = "UTC"),
      length.out = 10), as.POSIXct("2012-02-28 14:00:00", tz = "UTC")),
    value = c(1:3, 17:20, 5, 90, 13, 44),
    baseyear = factor(seq(3002L, 3012L))
  ))

  expect_identical(
    extract_baseyear(df1, "3004"),
    df1[3, ],
    ignore_attr = "row.names"
  )

  expect_identical(
    extract_baseyear(df1, c("3007", "3010")),
    df1[c(6, 9), ],
    ignore_attr = "row.names"
  )

})


test_that("event probabilities works", {

  skip("event probabilities not implemented")

  df1 = data.frame(alt = "A", path = "A",
    date = c(seq(as.POSIXct("1981-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
      length.out = 10L), as.POSIXct("2020-02-29 14:00:00")),
    event = seq_len(11L), value = NA)

  ew1 = data.frame(alt = "A", event = seq_len(11L), path = "A",
    wypercentile = c(0.1, 0.05, 0.15, 0.1, 0.01,
      0.14, 0.15, 0.05, 0.06, 0.09, 0.1))

  ew2 = data.frame(alt = "A", event = seq_len(11L), path = "A",
    wypercentile = c(10, 5, 15, 10, 1, 14, 15, 5, 6, 9, 10))


  check1 = df1
  check1["wypercentile"] = 1 / nrow(check1)
  check2 = df1
  check2["wypercentile"] = cumsum(rep(1, nrow(check1)) / nrow(check1))

  expect_identical(assign_probabilities(df1, cumulative = FALSE), check1)
  expect_identical(assign_probabilities(df1, cumulative = TRUE), check2)

  check3 = df1
  check3["wypercentile"] = cumsum(ew1$wypercentile / sum(ew1$wypercentile))
  expect_identical(assign_probabilities(df1, ew1), check3)
  expect_identical(assign_probabilities(df1, ew2), check3)

  # multi event
  df2 = data.frame(alt = "B", path = "B",
    date = c(seq(as.POSIXct("1981-05-01 12:00:00",
      tz = "UTC"), as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
      length.out = 10L), as.POSIXct("2020-02-29 14:00:00")),
    event = seq_len(11L), value = NA)

  ew3 = data.frame(alt = "B", event = seq_len(11L), path = "B",
    wypercentile = c(10, 5, 15, 10, 1, 14, 15, 5, 6, 9, 10))

  df3 = rbind(df1, df2)
  ew4 = rbind(ew1, ew3)

  check4 = df3
  check4["wypercentile"] = rep(check3$wypercentile, 2)
  expect_identical(assign_probabilities(df3, ew4), check4)

})