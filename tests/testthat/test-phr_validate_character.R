test_that("phr_validate_character passes for character input", {
  expect_no_error(
    phr_validate_character("text", soft = FALSE)
  )

  expect_no_error(
    phr_validate_character(c("a", "b"), soft = FALSE)
  )
})

test_that("phr_validate_character errors on non-character input when soft=FALSE", {
  expect_error(
    phr_validate_character(123, soft = FALSE)
  )
})
