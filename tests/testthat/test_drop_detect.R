library(testthat)
library(dropout)

test_that("drop_detect identifies dropouts correctly", {
  sample_data <- data.frame(
    id = 1:3,
    q1 = c(1, 2, 3),
    q2 = c(3, NA, NA),
    q3 = c(1, 2, NA)
  )

  result <- drop_detect(sample_data)

  expect_equal(result$drop, c(FALSE, FALSE, TRUE))
})

test_that("drop_detect handles all-NA column correctly", {
  sample_data <- data.frame(
    q1 = c(NA, NA, NA),
    q2 = c(NA, NA, NA)
  )

  result <- drop_detect(sample_data)

  expect_equal(result$drop, c(TRUE, TRUE, TRUE))
})
