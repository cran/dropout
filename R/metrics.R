#' Generate a summary of metrics for the dataset
#'
#' This function calculates a variety of metrics on the provided data and a corresponding output (`c_output`).
#' These metrics include drop rate, section NA, section length, single NA, and the number of complete rows.
#'
#' @param data A dataframe containing the dataset.
#' @param c_output A list or dataframe containing the computed output that helps generate the metrics.
#'
#' @return A dataframe summarizing the computed metrics for each column.
#' @keywords internal
metrics_summary <- function(data, c_output) {
  column <- names(data)
  drop <- metric_drop(c_output)
  sec_na <- metric_sec_na(c_output)
  sec_length <- metric_sec_length(c_output)
  single_na <- metric_single_na(c_output)
  na <- metric_na(drop, sec_na, single_na)
  complete <- metric_complete(na, data)


  data.frame(
    column,
    drop,
    sec_na,
    sec_length,
    single_na,
    na,
    complete
  )
}

#' Compute drop metric for each column
#'
#' This function computes the number of drops for each column based on the `c_output` data.
#'
#' @param c_output A list containing output data to compute the drop metric.
#' @return A vector of drop values.
#' @keywords internal
metric_drop <- function(c_output) {
  drop <- rowSums(sapply(c_output, function(x) as.numeric(x[[1]])))
}

#' Compute section length metric for each column
#'
#' This function calculates the mode (most common value) of the left-out section length for each column.
#'
#' @param c_output A list containing output data to compute the section length metric.
#' @return A vector of section length values.
#' @keywords internal
metric_sec_length <- function(c_output) {
  rs <- sapply(c_output, function(x) as.numeric(x[[2]]))
  spl <- split(rs, seq(nrow(rs)))
  seq <- lapply(spl, function(vec) vec[vec > 0])
  sec <- sapply(seq, function(vec) vec[which.max(tabulate(match(vec, unique(vec))))])
  sec_length <- ifelse(is.na(sec), 0, sec)
}

#' Compute section NA metric for each column
#'
#' This function calculates the total number of section NAs for each column in the dataset.
#'
#' @param c_output A list containing output data to compute the section NA metric.
#' @return A vector of section NA values.
#' @keywords internal
metric_sec_na <- function(c_output) {
  sec_na <- rowSums(sapply(c_output, function(x) as.numeric(x[[3]])))
}

#' Compute single NA metric for each column
#'
#' This function calculates the total number of single NAs for each column.
#'
#' @param c_output A list containing output data to compute the single NA metric.
#' @return A vector of single NA values.
#' @keywords internal
metric_single_na <- function(c_output) {
  single_na <- rowSums(sapply(c_output, function(x) as.numeric(x[[4]])))
}

#' Compute total NA count for each column
#'
#' This function calculates the total NA count for each column by summing the drop, section NA, and single NA metrics.
#'
#' @param drop A vector of drop counts for each column.
#' @param sec_na A vector of section NA counts for each column.
#' @param single_na A vector of single NA counts for each column.
#' @return A vector of total NA counts.
#' @keywords internal
metric_na <- function(drop, sec_na, single_na) {
  na <- cumsum(drop) + sec_na + single_na
}

#' Compute completeness of each column
#'
#' This function calculates the completeness of each column in the dataset by subtracting the NA count from the total number of rows.
#'
#' @param na A vector of NA counts for each column.
#' @param data The original dataset.
#' @return A vector of completeness values (proportions) for each column.
#' @keywords internal
metric_complete <- function(na, data) {
  complete <- round(1 - (na / nrow(data)), 2)
}




#' Detect dropped observations and columns
#'
#' This function identifies rows and columns with dropped values in the dataset based on the `c_output`.
#'
#' @param data The original dataset.
#' @param c_output A list containing output data to detect dropped values.
#' @return A dataframe indicating the dropped rows and columns.
#' @keywords internal
metrics_detect <- function(data, c_output) {
  drop <- metric_drop_id(c_output)
  drop_index <- metric_drop_index(c_output)
  column <- metric_column(c_output, drop_index, data)

  data.frame(drop, drop_index, column)
}

#' Compute drop indicator for each row
#'
#' This function computes a logical vector indicating whether each row in the dataset has been dropped.
#'
#' @param c_output A list containing output data to compute the drop indicator.
#' @return A logical vector indicating dropped rows (TRUE = dropped).
#' @keywords internal
metric_drop_id <- function(c_output) {
  drop_p <- ifelse(c_output[[1]] == 1, TRUE, FALSE)
}

#' Compute drop index for each row
#'
#' This function returns the index of the dropped column for each row, or `NA` if no column was dropped.
#'
#' @param c_output A list containing output data to compute the drop index.
#' @return A vector of drop indices (or NA if no drop occurred).
#' @keywords internal
metric_drop_index <- function(c_output) {
  drop_index <- ifelse(c_output[[2]] == 0, NA, c_output[[2]])
}

#' Compute column name for dropped observations
#'
#' This function returns the column names corresponding to the dropped values in the dataset.
#'
#' @param c_output A list containing output data to detect dropped columns.
#' @param drop_index A vector indicating the index of the dropped column for each row.
#' @param data The original dataset.
#' @return A vector of column names corresponding to dropped values.
#' @keywords internal
metric_column <- function(c_output, drop_index, data) {
  drop_column <- ifelse(!is.na(drop_index), names(data)[drop_index], NA)
}
