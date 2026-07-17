test_that("phr_assert passes when condition is TRUE", {
  expect_no_error(
    phr_assert(TRUE, "Should not error")
  )

  expect_no_error(
    phr_assert(1 == 1, "Should not error")
  )
})

test_that("phr_assert errors when condition is FALSE", {
  expect_error(
    phr_assert(FALSE, "Assertion failed"),
    regexp = "Assertion failed"
  )
})

test_that("phr_assert includes origin in error", {
  expect_error(
    phr_assert(FALSE, "Assertion failed", origin = "test_func"),
    regexp = "test_func"
  )
})

test_that("phr_assert includes hint in error", {
  expect_error(
    phr_assert(FALSE, "Assertion failed", hint = "Check input"),
    regexp = "Check input"
  )
})

test_that("phr_assert errors on NA condition", {
  expect_error(
    phr_assert(NA, "NA is not TRUE"),
    regexp = "NA is not TRUE"
  )
})
