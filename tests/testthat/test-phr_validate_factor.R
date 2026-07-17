test_that("phr_validate_factor passes for factor input", {
  expect_no_error(
    phr_validate_factor(factor(c("a", "b")), soft = FALSE)
  )
})

test_that("phr_validate_factor errors on non-factor input when soft=FALSE", {
  expect_error(
    phr_validate_factor(c("a", "b"), soft = FALSE)
  )
})
