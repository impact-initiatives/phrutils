test_that("phr_parse_hhmm returns correct minutes for standard HH:MM strings", {
  expect_equal(phr_parse_hhmm("00:00"), 0L)
  expect_equal(phr_parse_hhmm("08:00"), 480L)
  expect_equal(phr_parse_hhmm("10:00"), 600L)
  expect_equal(phr_parse_hhmm("18:00"), 1080L)
  expect_equal(phr_parse_hhmm("23:59"), 1439L)
})

test_that("phr_parse_hhmm handles single-digit hour", {
  expect_equal(phr_parse_hhmm("8:30"), 510L)
})

test_that("phr_parse_hhmm returns numeric input unchanged", {
  expect_equal(phr_parse_hhmm(480), 480)
})

test_that("phr_parse_hhmm errors on non-HH:MM character string", {
  expect_error(phr_parse_hhmm("2024-01-01"))
  expect_error(phr_parse_hhmm("hello"))
  expect_error(phr_parse_hhmm("1800"))
})

test_that("phr_parse_hhmm errors on out-of-range values", {
  expect_error(phr_parse_hhmm("25:00"))
  expect_error(phr_parse_hhmm("12:60"))
})
