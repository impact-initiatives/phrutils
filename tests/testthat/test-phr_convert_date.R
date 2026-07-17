test_that("phr_convert_date converts ISO ymd strings correctly", {
  x <- c("2025-07-13", "2025-01-01")
  res <- phr_convert_date(x)
  expect_s3_class(res, "Date")
  expect_equal(res, as.Date(x))
})

test_that("phr_convert_date parses dmy and mdy formats", {
  x <- c("13/07/2025", "07-13-2025")
  res <- phr_convert_date(x)
  expect_equal(res, as.Date(c("2025-07-13", "2025-07-13")))
})

test_that("phr_convert_date handles mixed formats", {
  x <- c("2025-07-13", "13/07/2025", "07/13/2025")
  res <- phr_convert_date(x)
  expect_equal(res, rep(as.Date("2025-07-13"), 3))
})

test_that("phr_convert_date handles POSIXct and POSIXlt inputs", {
  x <- as.POSIXct("2025-07-13 12:00:00", tz = "UTC")
  res <- phr_convert_date(x)
  expect_s3_class(res, "Date")
  expect_equal(res, as.Date("2025-07-13"))

  y <- as.POSIXlt(x)
  res2 <- phr_convert_date(y)
  expect_equal(res2, as.Date("2025-07-13"))
})

test_that("phr_convert_date handles numeric days since epoch", {
  x <- as.numeric(as.Date("2025-07-13"))
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2025-07-13"))

})

test_that("phr_convert_date handles Excel serial numbers (post-1900)", {
  # Correct Excel serial number for 2023-01-01 is 44927
  x <- 44927
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2023-01-01"))
})

test_that("phr_convert_date handles numeric-like character Excel serial", {
  x <- "44927"  # Excel serial for 2023-01-01
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2023-01-01"))
})

test_that("phr_convert_date handles numeric-like character Unix days", {
  x <- "20000"  # Unix epoch -> 2024-10-04
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2024-10-04"))
})

test_that("phr_convert_date preserves NA values", {
  x <- c("2025-07-13", NA, "2025-08-01")
  res <- phr_convert_date(x)
  expect_true(is.na(res[2]))
  expect_equal(res[c(1,3)], as.Date(c("2025-07-13", "2025-08-01")))
})

test_that("phr_convert_date strips time components and timezones", {
  x <- c(
    "2025-07-13 14:22:10",
    "2025-07-13T14:22:10Z",
    "2025-07-13 14:22:10 UTC"
  )
  res <- phr_convert_date(x)
  expect_equal(unique(res), as.Date("2025-07-13"))
})

test_that("phr_convert_date errors on unparseable values", {
  x <- c("2025-07-13", "not-a-date")

  expect_error(
    phr_convert_date(x),
    regexp = "Could not convert values to Date"
  )
})

test_that("phr_convert_date handles vectors of length 1", {
  x <- "2025-07-13"
  res <- phr_convert_date(x)
  expect_equal(res, as.Date("2025-07-13"))
})

test_that("phr_convert_date supports mixed NA and bad values but still errors properly", {
  x <- c(NA, "bad-date")

  expect_error(
    phr_convert_date(x),
    regexp = "bad-date"
  )
})

test_that("phr_convert_date trims whitespace correctly", {
  x <- c(" 2025-07-13 ", "\t2025-08-01")
  res <- phr_convert_date(x)
  expect_equal(res, as.Date(c("2025-07-13", "2025-08-01")))
})
