test_that("phr_message creates formatted informational message", {
  expect_no_error(
    phr_message("Test message", origin = "test_function")
  )
})

test_that("phr_message works without origin", {
  expect_no_error(
    phr_message("Simple message")
  )
})
