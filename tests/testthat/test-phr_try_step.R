test_that("phr_try_step executes successfully", {
  result <- phr_try_step({
    42
  }, step = "Test Step")

  expect_equal(result, 42)
})

test_that("phr_try_step returns error list on failure", {
  result <- phr_try_step({
    stop("Step error")
  }, step = "Test Step")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_equal(result$step, "Test Step")
})

test_that("phr_try_step includes hint in error context", {
  result <- phr_try_step({
    stop("Step error")
  }, step = "Test Step", hint = "Check input")

  expect_equal(result$hint, "Check input")
})

test_that("phr_try_step always uses on_error='return'", {
  # Should return error list, not throw
  result <- phr_try_step({
    stop("Error")
  }, step = "Test")

  expect_true(is.list(result))
  expect_false(result$success)
})

test_that("multiple steps can be tracked in nested try blocks", {
  result <- phr_try({
    r1 <- phr_try_step({ 1 }, step = "Init")
    if (phr_failed(r1)) { r1 } else {
      r2 <- phr_try_step({ 2 }, step = "Process")
      if (phr_failed(r2)) { r2 } else {
        r3 <- phr_try_step({ stop("Final error") }, step = "Finalize")
        r3
      }
    }
  }, on_error = "return", origin = "MultiStep")

  expect_true(phr_failed(result))
  expect_equal(result$step, "Finalize")
})
