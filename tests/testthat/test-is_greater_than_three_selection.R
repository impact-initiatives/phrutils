test_that("is_greater_than_three_selection identifies >3 selections", {
  df <- tibble::tibble(
    sm_col = c(
      "a b",
      "a",
      "a b c d",
      "a b c d e"
    )
  )

  result <- is_greater_than_three_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("is_greater_than_three_selection handles exactly three selections", {
  df <- tibble::tibble(
    sm_col = c("a b c", "a b c d")
  )

  result <- is_greater_than_three_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, TRUE))
})
