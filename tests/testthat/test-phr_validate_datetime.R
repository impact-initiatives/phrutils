test_that("phr_validate_datetime() — accepts POSIXct input", {
  x <- as.POSIXct("2025-10-16 14:32:00", tz = "UTC")
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts POSIXlt input", {
  x <- as.POSIXlt("2025-10-16 14:32:00", tz = "UTC")
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts datetime string with time component", {
  x <- "2025-10-16 14:32:00"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts ISO 8601 datetime string", {
  x <- "2025-10-16T14:32:00Z"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — accepts datetime string with HH:MM only", {
  x <- "2025-10-16 14:32"
  expect_true(phr_validate_datetime(x, soft = TRUE))
})

test_that("phr_validate_datetime() — rejects bare Date object (soft=TRUE, warns)", {
  x <- as.Date("2025-10-16")
  expect_warning(
    result <- phr_validate_datetime(x, soft = TRUE)
  )
  expect_false(result)
})

test_that("phr_validate_datetime() — rejects date-only string (soft=TRUE, warns)", {
  x <- "2025-10-16"
  expect_warning(
    result <- phr_validate_datetime(x, soft = TRUE)
  )
  expect_false(result)
})

test_that("phr_validate_datetime() — rejects bare Date object (soft=FALSE, errors)", {
  x <- as.Date("2025-10-16")
  expect_error(
    phr_validate_datetime(x, soft = FALSE)
  )
})

test_that("phr_validate_datetime() — rejects non-date string (soft=FALSE, errors)", {
  x <- "not-a-datetime"
  expect_error(
    phr_validate_datetime(x, soft = FALSE)
  )
})

test_that("phr_validate_datetime() — rejects NULL input", {
  expect_error(
    phr_validate_datetime(NULL, soft = FALSE)
  )
})
