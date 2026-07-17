test_that("get_color_palette supports 'ipc' type", {
  result <- get_color_palette("ipc")
  expect_length(result, 5)
})

test_that("get_color_palette supports 'water' type", {
  result <- get_color_palette("water")
  expect_length(result, 3)
})

test_that("get_color_palette supports 'sanitation' type", {
  result <- get_color_palette("sanitation")
  expect_length(result, 3)
})

test_that("get_color_palette supports 'hygiene' type", {
  result <- get_color_palette("hygiene")
  expect_length(result, 3)
})

test_that("get_color_palette 'water' with n > 3 interpolates", {
  result <- get_color_palette("water", n = 6)
  expect_length(result, 6)
})

test_that("get_color_palette 'sanitation' with reverse returns reversed colors", {
  fwd <- get_color_palette("sanitation")
  rev_result <- get_color_palette("sanitation", reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[3]))
})

test_that("get_color_palette rejects invalid type", {
  expect_error(get_color_palette("wash"))
  expect_error(get_color_palette("invalid_type"))
})
