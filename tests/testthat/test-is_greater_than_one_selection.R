test_that("is_greater_than_one_selection identifies multiple selections", {
  df <- tibble::tibble(
    sm_col = c(
      "option_a option_b",
      "option_c",
      "option_d option_e option_f",
      ""
    )
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("is_greater_than_one_selection handles single selections", {
  df <- tibble::tibble(
    sm_col = c("option_a", "option_b", "option_c")
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(FALSE, FALSE, FALSE))
})

test_that("is_greater_than_one_selection handles empty dataset", {
  df <- tibble::tibble(sm_col = character())

  result <- is_greater_than_one_selection(df, "sm_col")

  # Expect an empty result (could be logical(0), character(0), or tibble with 0 rows)
  expect_equal(length(result), 0)
  # Or if it returns a tibble:
  # expect_equal(nrow(result), 0)
})

test_that("is_greater_than_one_selection errors on missing column", {
  df <- tibble::tibble(other_col = "value")

  expect_error(
    is_greater_than_one_selection(df, "sm_col")
  )
})

test_that("is_greater_than_one_selection handles factor columns", {
  df <- tibble::tibble(
    sm_col = factor(c("a b", "c", "d e"))
  )

  result <- is_greater_than_one_selection(df, "sm_col")

  expect_equal(unname(result), c(TRUE, FALSE, TRUE))
})
