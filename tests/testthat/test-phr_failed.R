test_that("phr_failed returns TRUE for failed result", {
  failed_result <- list(success = FALSE, error = "Test error")

  expect_true(phr_failed(failed_result))
})

test_that("phr_failed returns FALSE for successful result", {
  success_result <- list(success = TRUE)

  expect_false(phr_failed(success_result))
})

test_that("phr_failed returns FALSE for non-list result", {
  expect_false(phr_failed(42))
  expect_false(phr_failed("text"))
  expect_false(phr_failed(NULL))
})

test_that("phr_failed returns FALSE for list without success field", {
  result <- list(error = "Something")

  expect_false(phr_failed(result))
})

test_that("phr_failed returns FALSE for success = TRUE", {
  result <- list(success = TRUE, data = 42)

  expect_false(phr_failed(result))
})
