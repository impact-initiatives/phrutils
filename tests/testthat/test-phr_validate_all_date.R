test_that("phr_validate_all_date passes when all elements are Date", {
  expect_no_error(
    phr_validate_all_date(rep(Sys.Date(), 3), soft = FALSE)
  )
})
