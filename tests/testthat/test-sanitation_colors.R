test_that("sanitation_colors returns 3 hex colors by default (unnamed)", {
  result <- sanitation_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("sanitation_colors returns correct default hex values", {
  result <- sanitation_colors()

  expect_equal(result, c("#75599F", "#A08DBD", "#CBC1DB"))
})

test_that("sanitation_colors respects n <= 3 by truncating", {
  result <- sanitation_colors(n = 2)

  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("#75599F", "#A08DBD"))
})

test_that("sanitation_colors interpolates when n > 3", {
  result <- sanitation_colors(n = 5)

  expect_type(result, "character")
  expect_length(result, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))

  # sanity: gradient includes the endpoints used by colorRampPalette()
  expect_equal(result[1], "#532F87")
  expect_equal(result[5], "#CBC1DB")
})

test_that("sanitation_colors reverses order when reverse = TRUE (default n)", {
  fwd <- sanitation_colors()
  rev_result <- sanitation_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("sanitation_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- sanitation_colors(n = 4)
  rev_result <- sanitation_colors(n = 4, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})
