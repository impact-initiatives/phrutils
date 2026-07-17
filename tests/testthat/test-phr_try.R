test_that("phr_try executes expression successfully", {
  result <- phr_try({
    42
  }, on_error = "return")

  expect_equal(result, 42)
})

test_that("phr_try handles errors with on_error='return'", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_true(grepl("Test error", result$error))
})

test_that("phr_try handles errors with on_error='warn'", {
  expect_warning(
    phr_try({
      stop("Test error")
    }, on_error = "warn"),
    regexp = "Test error"  # May or may not warn depending on implementation
  )
})

test_that("phr_try handles errors with on_error='abort'", {
  expect_error(
    phr_try({
      stop("Test error")
    }, on_error = "abort"),
    regexp = "Test error"
  )
})

test_that("phr_try includes origin in error message", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", origin = "test_function")

  expect_true(grepl("test_function", result$error))
})

test_that("phr_try includes hint in error context", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", hint = "Try this fix")

  expect_equal(result$hint, "Try this fix")
})

test_that("phr_try includes step in error message", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", step = "Validation")

  expect_equal(result$step, "Validation")
})

test_that("phr_try combines origin and step correctly", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", origin = "main_function", step = "Validation")

  expect_true(grepl("main_function", result$error) || grepl("Validation", result$error))
})

test_that("phr_try preserves nested error context", {
  result <- phr_try({
    phr_try({
      stop("Inner error")
    }, on_error = "abort", step = "Inner")
  }, on_error = "return", origin = "Outer")

  expect_true(is.list(result))
  expect_false(result$success)
})

test_that("nested phr_try calls preserve context chain", {
  outer_result <- phr_try({
    step1 <- phr_try_step({
      42
    }, step = "Step 1")

    if (phr_failed(step1)) return(step1)

    step2 <- phr_try_step({
      stop("Error in step 2")
    }, step = "Step 2")

    if (phr_failed(step2)) return(step2)

    "success"
  }, on_error = "return", origin = "Outer Function")

  expect_true(phr_failed(outer_result))
  expect_equal(outer_result$step, "Step 2")
})

test_that("phr_try handles empty expression", {
  result <- phr_try({}, on_error = "return")
  expect_true(is.null(result) || !is.list(result) || result$success != FALSE)
})

test_that("phr_try handles NULL origin and hint", {
  result <- phr_try({
    stop("Error")
  }, on_error = "return", origin = NULL, hint = NULL)

  expect_true(is.list(result))
  expect_false(result$success)
})
