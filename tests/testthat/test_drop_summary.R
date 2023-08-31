library(testthat)
library(dropout)


test_that("drop_summary returns correct statistics for each column", {
  sample_data <- data.frame(
    q1 = c(1, 2, 3, NA, NA),
    q2 = c(1, NA, NA, NA, NA),
    q3 = c(1, 2, 3, 4, NA)
  )
  result <- drop_summary(sample_data)

  # Check statistics for column 'q1'
  expect_equal(result[result$column_name == 'q1', 'dropout'][[1]], 1)
  expect_equal(result[result$column_name == 'q1', 'drop_rate'][[1]], 0.2)
  expect_equal(result[result$column_name == 'q1', 'drop_na'][[1]], 1)

  # Check statistics for column 'q2'
  expect_equal(result[result$column_name == 'q2', 'dropout'][[1]], 0)
  expect_equal(result[result$column_name == 'q2', 'drop_rate'][[1]], 0.2)
  expect_equal(result[result$column_name == 'q2', 'drop_na'][[1]], 1)

  # Check statistics for column 'q3'
  expect_equal(result[result$column_name == 'q3', 'dropout'][[1]], 0)
  expect_equal(result[result$column_name == 'q3', 'drop_rate'][[1]], 0.2)
  expect_equal(result[result$column_name == 'q3', 'drop_na'][[1]], 1)
})

