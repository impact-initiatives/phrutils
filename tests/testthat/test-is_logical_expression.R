test_that("is_logical_expression accepts valid logical expressions", {

  valid_exprs <- list(
    "x > 5",
    "age <= 10 & sex == 'f'",
    "!is.na(weight)",
    "grepl('abc', name)",
    "grepl('a', name) & age > 5",
    "TRUE",
    "FALSE",
    "(x > 1 | y < 2) & z == 0",
    # New: %in% operator
    "x %in% c('a', 'b', 'c')",
    "status %in% c('active', 'pending')",
    "age %in% 1:100",
    # New: is_safely_coercible function
    "is_safely_coercible(x, 'numeric')",
    "is_safely_coercible(age, 'date')",
    # New: Base R type check functions
    "is.numeric(age)",
    "is.character(name)",
    "is.logical(flag)",
    "is.integer(count)",
    "is.double(value)",
    "is.factor(category)",
    "is.null(missing_val)",
    # Combined expressions with new constructs
    "x %in% c(1, 2, 3) & is.numeric(x)",
    "is.character(name) | is.na(name)",
    "is_safely_coercible(x, 'numeric') & !is.na(x)"
  )

  for (txt in valid_exprs) {
    expect_true(
      is_logical_expression(txt),
      info = paste("Expected valid logical expression:", txt)
    )
  }
})

test_that("is_logical_expression rejects non-logical but syntactically valid expressions", {

  invalid_exprs <- list(
    "'my name is Jack'",
    "42",
    "3.14",
    "mean(x)",
    "paste(a, b)",
    "x + y",
    "log(x)"
  )

  for (txt in invalid_exprs) {
    expect_false(
      is_logical_expression(txt),
      info = paste("Expected invalid logical expression:", txt)
    )
  }
})

test_that("is_logical_expression rejects malformed or unsupported calls", {

  invalid_calls <- list(
    quote(`{`(x > 1)),
    quote(if (x > 1) TRUE else FALSE),
    quote(function(x) x > 1),
    quote(return(x > 1))
  )

  for (expr in invalid_calls) {
    expect_false(
      is_logical_expression(expr),
      info = paste("Expected invalid logical expression:", deparse(expr))
    )
  }
})

test_that("is_logical_expression rejects atomic literals", {

  expect_false(is_logical_expression(quote("abc")))
  expect_false(is_logical_expression(quote(1)))
  expect_false(is_logical_expression(quote(NA)))
})
