test_that("phr_validate_no_missing passes when no NA values", {
  df <- tibble::tibble(
    a = 1:5,
    b = c("x", "y", "z", "w", "v")
  )

  expect_no_error(
    phr_validate_no_missing(df, c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_no_missing errors when NA values present and soft=FALSE", {
  df <- tibble::tibble(
    a = c(1, 2, NA, 4, 5)
  )

  expect_error(
    phr_validate_no_missing(df, "a", soft = FALSE)
  )
})
