test_that("phr_validate_date passes for Date input", {
  expect_no_error(
    phr_validate_date(Sys.Date(), soft = FALSE)
  )
})

test_that("phr_validate_date errors on non-Date input when soft=FALSE", {
  expect_error(
    phr_validate_date(5, soft = FALSE)
  )
})
