test_that("hygiene_colors returns 3 hex colors by default (unnamed)", {
  result <- hygiene_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("hygiene_colors returns correct default hex values", {
  result <- hygiene_colors()

  expect_equal(result, c("#33A46D", "#73C09A", "#B2DDC8"))
})

test_that("hygiene_colors interpolates when n > 3", {
  result <- hygiene_colors(n = 7)

  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("hygiene_colors reverses order when reverse = TRUE (default n)", {
  fwd <- hygiene_colors()
  rev_result <- hygiene_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("hygiene_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- hygiene_colors(n = 4)
  rev_result <- hygiene_colors(n = 4, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})
