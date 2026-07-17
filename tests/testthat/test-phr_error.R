test_that("phr_error creates formatted error message with origin", {
  expect_error(
    phr_error("Test error", origin = "test_function"),
    regexp = "test_function.*Test error"
  )
})

test_that("phr_error includes hint in error message", {
  expect_error(
    phr_error("Test error", hint = "Try this fix"),
    regexp = "Try this fix"
  )
})

test_that("phr_error works without origin or hint", {
  expect_error(
    phr_error("Simple error"),
    regexp = "Simple error"
  )
})

test_that("phr_error includes custom type in message", {
  expect_error(
    phr_error("Test error", type = "ValidationError"),
    regexp = "ValidationError"
  )
})

test_that("phr_error in test mode does not call shiny::req", {
  # Set test mode
  old_opt <- getOption("IPHRA_TEST_MODE")
  options(IPHRA_TEST_MODE = TRUE)

  # Should error without calling shiny::req
  expect_error(
    phr_error("Test error"),
    regexp = "Test error"
  )

  # Restore option
  options(IPHRA_TEST_MODE = old_opt)
})

test_that("phr_error handles very long messages", {
  long_msg <- paste(rep("word", 1000), collapse = " ")

  expect_error(
    phr_error(long_msg),
    regexp = "word"
  )
})

test_that("error functions handle special characters in messages", {
  expect_error(
    phr_error("Error with [brackets] and (parens) and $symbols"),
    regexp = "brackets"
  )
})
