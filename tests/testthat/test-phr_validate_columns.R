test_that("phr_validate_columns passes when columns exist", {
  df <- tibble::tibble(a = 1, b = 2, c = 3)

  expect_no_error(
    phr_validate_columns(df, c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_columns errors when columns missing and soft=FALSE", {
  df <- tibble::tibble(a = 1, b = 2)

  expect_error(
    phr_validate_columns(df, c("a", "c"), soft = FALSE)
  )
})
