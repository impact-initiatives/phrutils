test_that("phr_validate_vector_length passes for valid length", {
  expect_no_error(
    phr_validate_vector_length(c(1, 2, 3), min_length = 1, soft = FALSE)
  )
})

test_that("phr_validate_vector_length errors when length too short and soft=FALSE", {
  expect_error(
    phr_validate_vector_length(c(1), min_length = 2, soft = FALSE)
  )
})

test_that("phr_validate_vector_length checks exact length", {
  expect_no_error(
    phr_validate_vector_length(c(1, 2, 3), exact_length = 3, soft = FALSE)
  )

  expect_error(
    phr_validate_vector_length(c(1, 2), exact_length = 3, soft = FALSE)
  )
})
