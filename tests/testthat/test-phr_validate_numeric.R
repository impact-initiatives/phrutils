test_that("phr_validate_numeric passes for numeric input", {
  expect_no_error(
    phr_validate_numeric(123, soft = FALSE)
  )

  expect_no_error(
    phr_validate_numeric(c(1, 2, 3), soft = FALSE)
  )
})

test_that("phr_validate_numeric errors on non-numeric input when soft=FALSE", {
  expect_error(
    phr_validate_numeric("text", soft = FALSE)
  )
})

test_that("validators with soft=TRUE warn instead of error", {
  # This tests the soft validation mode behavior
  # When soft=TRUE, validators should warn but not error
  expect_warning(
    phr_validate_numeric("text", soft = TRUE),
    regexp = "Ensure input is of type"  # May or may not warn depending on implementation
  )
})

test_that("validators with soft=FALSE error on invalid input", {
  expect_error(
    phr_validate_numeric("text", soft = FALSE)
  )
})
