test_that("phr_validate_non_negative passes for non-negative values", {
  df <- tibble::tibble(
    value = c(0, 1, 2, 3, 4)
  )

  expect_no_error(
    phr_validate_non_negative(df, "value", soft = FALSE)
  )
})

test_that("phr_validate_non_negative errors on negative values when soft=FALSE", {
  df <- tibble::tibble(
    value = c(1, -1, 2)
  )

  expect_error(
    phr_validate_non_negative(df, "value", soft = FALSE)
  )
})
