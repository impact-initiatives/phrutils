test_that("phr_validate_choice passes for valid choice", {
  expect_no_error(
    phr_validate_choice("a", choices = c("a", "b", "c"), soft = FALSE)
  )
})

test_that("phr_validate_choice errors on invalid choice when soft=FALSE", {
  expect_warning(
    phr_validate_choice("d", choices = c("a", "b", "c"), soft = FALSE)
  )
})
