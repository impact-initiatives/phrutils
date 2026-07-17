test_that("phr_validate_column_types passes when types match", {
  df <- tibble::tibble(
    a = 1:5,
    b = c("x", "y", "z", "w", "v"),
    c = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  expected_types <- list(
    a = "integer",
    b = "character",
    c = "logical"
  )

  expect_no_error(
    phr_validate_column_types(df, expected_types, soft = FALSE)
  )
})

test_that("phr_validate_column_types errors when types don't match and soft=FALSE", {
  df <- tibble::tibble(
    a = c("1", "2", "3")  # character, not numeric
  )

  expected_types <- list(a = "numeric")

  expect_error(
    phr_validate_column_types(df, expected_types, soft = FALSE)
  )
})
