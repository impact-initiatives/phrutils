test_that("phr_notify handles message type", {
  # Should not error when called outside Shiny
  expect_no_error(
    phr_notify("Test message", type = "message")
  )
})

test_that("phr_notify handles warning type", {
  expect_no_error(
    phr_notify("Test warning", type = "warning")
  )
})

test_that("phr_notify handles error type", {
  expect_no_error(
    phr_notify("Test error", type = "error")
  )
})

test_that("phr_notify validates type argument", {
  expect_error(
    phr_notify("message", type = "invalid"),
    regexp = "should be one of"
  )
})
