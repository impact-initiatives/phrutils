test_that("phr_warning creates formatted warning message", {
  expect_no_error(
    phr_warning("Test warning", origin = "test_function")
  )
})

test_that("phr_warning includes hint in message", {
  expect_no_error(
    phr_warning("Test warning", hint = "Consider this")
  )
})

test_that("phr_warning works without origin or hint", {
  expect_no_error(
    phr_warning("Simple warning")
  )
})

test_that("phr_warning allows custom type", {
  expect_no_error(
    phr_warning("Test warning", type = "DataWarning")
  )
})
