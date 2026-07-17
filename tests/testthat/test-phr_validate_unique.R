test_that("phr_validate_unique passes when all values are unique", {
  df <- tibble::tibble(
    id = 1:5
  )

  expect_no_error(
    phr_validate_unique(df, "id", soft = FALSE)
  )
})

test_that("phr_validate_unique errors when duplicates exist and soft=FALSE", {
  df <- tibble::tibble(
    id = c(1, 2, 2, 3, 4)
  )

  expect_error(
    phr_validate_unique(df, "id", soft = FALSE)
  )
})
