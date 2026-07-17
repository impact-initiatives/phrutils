test_that("phr_validate_dataframe passes for data.frame input", {
  expect_no_error(
    phr_validate_dataframe(data.frame(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_dataframe passes for tibble input", {
  expect_no_error(
    phr_validate_dataframe(tibble::tibble(a = 1), soft = FALSE)
  )
})

test_that("phr_validate_dataframe errors on non-dataframe input when soft=FALSE", {
  expect_error(
    phr_validate_dataframe(list(a = 1), soft = FALSE)
  )
})
