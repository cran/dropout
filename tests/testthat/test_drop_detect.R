library(testthat)
library(dropout)

test_that("drop_detect identifies dropouts correctly", {
  sample_data <- data.frame(
    id = 1:3,
    q1 = c(1, 2, 3),
    q2 = c(3, NA, NA),
    q3 = c(1, 2, NA)
  )

  result <- drop_detect(sample_data, "q3")

  expect_equal(result$dropout, c(FALSE, FALSE, TRUE))
})

test_that("drop_detect works when last_col is specified as an index", {
  sample_data <- data.frame(
    q1 = c(1, 2, 3),
    q2 = c(3, NA, NA),
    q3 = c(1, 2, NA)
  )

  result <- drop_detect(sample_data, 3)

  expect_equal(result$dropout, c(FALSE, FALSE, TRUE))
})

test_that("drop_detect handles all-NA column correctly", {
  sample_data <- data.frame(
    q1 = c(NA, NA, NA),
    q2 = c(NA, NA, NA)
  )

  result <- drop_detect(sample_data, "q2")

  expect_equal(result$dropout, c(TRUE, TRUE, TRUE))
})


test_that("drop_detect handles missing last_col gracefully", {
  sample_data <- data.frame(
    q1 = c(1, 2, 3),
    q2 = c(3, NA, NA)
  )

  expect_error(drop_detect(sample_data, "q5"))
})



