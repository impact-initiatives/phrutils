test_that("phr_validate_all_factor passes when all elements are from valid levels", {
  f <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))

  expect_no_error(
    phr_validate_all_factor(f, allowed_levels = c("a", "b", "c"), soft = FALSE)
  )
})
