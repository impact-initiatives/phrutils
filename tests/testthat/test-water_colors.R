test_that("water_colors returns 3 hex colors by default (unnamed)", {
  result <- water_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("water_colors returns correct default hex values", {
  result <- water_colors()

  expect_equal(result, c("#53B4DB", "#89CBE6", "#BEE3F2"))
})

test_that("water_colors respects n <= 3 by truncating", {
  result <- water_colors(n = 2)

  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("#53B4DB", "#89CBE6"))
})

test_that("water_colors interpolates when n > 3", {
  result <- water_colors(n = 6)

  expect_type(result, "character")
  expect_length(result, 6)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))

  # sanity: gradient includes the endpoints used by colorRampPalette()
  expect_equal(result[1], "#28A1D2")
  expect_equal(result[6], "#BEE3F2")
})

test_that("water_colors reverses order when reverse = TRUE (default n)", {
  fwd <- water_colors()
  rev_result <- water_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("water_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- water_colors(n = 6)
  rev_result <- water_colors(n = 6, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})
