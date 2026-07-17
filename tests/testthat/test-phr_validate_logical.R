test_that("phr_validate_logical passes for logical input", {
  expect_no_error(
    phr_validate_logical(TRUE, soft = FALSE)
  )

  expect_no_error(
    phr_validate_logical(c(TRUE, FALSE), soft = FALSE)
  )
})

test_that("phr_validate_logical errors on non-logical input when soft=FALSE", {
  expect_error(
    phr_validate_logical(123, soft = FALSE)
  )
})
