library(testthat)
library(dropout)


test_that("drop_summary returns correct statistics for each column", {
  sample_data <- data.frame(
    q1 = c(1, 2, 3, NA, NA),
    q2 = c(1, NA, NA, NA, NA),
    q3 = c(1, 2, 3, 4, NA)
  )
  result <- drop_summary(sample_data)

  # Check statistics for drop output
  expect_equal(result[result$column == "q1", "drop"][[1]], 1)
  expect_equal(result[result$column == "q2", "drop"][[1]], 0)
  expect_equal(result[result$column == "q3", "drop"][[1]], 0)

  # Check statistics for sec_na output
  expect_equal(result[result$column == "q1", "sec_na"][[1]], 1)
  expect_equal(result[result$column == "q2", "sec_na"][[1]], 1)
  expect_equal(result[result$column == "q3", "sec_na"][[1]], 0)

  # Check statistics for sec_length output
  expect_equal(result[result$column == "q1", "single_na"][[1]], 0)
  expect_equal(result[result$column == "q2", "single_na"][[1]], 2)
  expect_equal(result[result$column == "q3", "single_na"][[1]], 0)

  # Check statistics for na output
  expect_equal(result[result$column == "q1", "na"][[1]], 2)
  expect_equal(result[result$column == "q2", "na"][[1]], 4)
  expect_equal(result[result$column == "q3", "na"][[1]], 1)

  # Check statistics for na output
  expect_equal(result[result$column == "q1", "complete"][[1]], 0.6)
  expect_equal(result[result$column == "q2", "complete"][[1]], 0.2)
  expect_equal(result[result$column == "q3", "complete"][[1]], 0.8)
})
