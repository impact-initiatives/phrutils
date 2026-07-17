test_that("phr_validate_list passes for list input", {
  expect_no_error(
    phr_validate_list(list(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_list errors on non-list input when soft=FALSE", {
  expect_error(
    phr_validate_list(c(1, 2, 3), soft = FALSE)
  )
})
