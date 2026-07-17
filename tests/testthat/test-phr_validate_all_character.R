test_that("phr_validate_all_character passes when all elements are character", {
  expect_no_error(
    phr_validate_all_character(c("a", "b", "c"), soft = FALSE)
  )
})
