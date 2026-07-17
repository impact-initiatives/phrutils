test_that("phr_validate_not_null passes for non-null values", {
  expect_no_error(
    phr_validate_not_null(123, soft = FALSE)
  )

  expect_no_error(
    phr_validate_not_null("text", soft = FALSE)
  )
})

test_that("phr_validate_not_null errors on NULL when soft=FALSE", {
  expect_error(
    phr_validate_not_null(NULL, soft = FALSE)
  )
})
