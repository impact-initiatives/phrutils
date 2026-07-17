test_that("ipc_colors returns 5 unnamed colors by default", {
  result <- ipc_colors()
  expect_length(result, 5)
  expect_null(names(result))
})

test_that("ipc_colors returns correct hex values ordered worst to best (P5 to P1)", {
  result <- ipc_colors()
  expect_equal(result[1], "#8C0000")  # P5 Famine (worst)
  expect_equal(result[2], "#FF0000")  # P4 Emergency
  expect_equal(result[3], "#FF9900")  # P3 Crisis
  expect_equal(result[4], "#FFEE00")  # P2 Stressed
  expect_equal(result[5], "#A1FE8D")  # P1 Minimal (best)
})

test_that("ipc_colors respects n parameter", {
  result <- ipc_colors(n = 3)
  expect_length(result, 3)
  expect_equal(result[1], "#8C0000")  # first 3 worst colors
})

test_that("ipc_colors with n = 4 returns worst 4 colors", {
  result <- ipc_colors(n = 4)
  expect_length(result, 4)
  expect_equal(result[1], "#8C0000")  # P5
  expect_equal(result[4], "#FFEE00")  # P2
})

test_that("ipc_colors interpolates when n > 5", {
  result <- ipc_colors(n = 8)
  expect_length(result, 8)
})

test_that("ipc_colors reverses order when reverse = TRUE", {
  fwd <- ipc_colors()
  rev_result <- ipc_colors(reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[5]))
  expect_equal(unname(rev_result[5]), unname(fwd[1]))
})
