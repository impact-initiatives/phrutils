test_that("phr_validate_all_logical passes when all elements are logical", {
  expect_no_error(
    phr_validate_all_logical(c(TRUE, FALSE, TRUE), soft = FALSE)
  )
})
