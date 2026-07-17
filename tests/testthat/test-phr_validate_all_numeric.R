test_that("phr_validate_all_numeric passes when all elements are numeric", {
  expect_no_error(
    phr_validate_all_numeric(c(1, 2, 3), soft = FALSE)
  )
})

test_that("phr_validate_all_numeric errors when some elements are not numeric", {
  expect_error(
    phr_validate_all_numeric(c(1, "g", 3), soft = FALSE)
  )
})
