#' Prepare data for C API: list of row-vectors with NA indicators
#'
#' This function prepares the data to be used by a C API by transforming each row of the dataframe into a vector
#' of 0s and 1s, where 0 indicates a non-NA value and 1 indicates an NA value.
#'
#' @param data A dataframe to be prepared for the C API.
#'
#' @return A list of row-vectors, where each vector contains 0s (non-NA) and 1s (NA).
#' @keywords internal
c_prepare <- function(data) {
  check_input(data)

  num <- sapply(data, function(col) as.integer(ifelse(is.na(col), 1, 0)))
  spl <- split(num, seq(nrow(num)))
}

#' Check that the input is a valid dataframe
#'
#' This function checks if the input is a valid dataframe with at least two rows and two columns.
#' It throws an error if the input does not meet these conditions.
#'
#' @param data The input to be checked.
#'
#' @return NULL. The function stops with an error message if the input is not a valid dataframe.
#' @keywords internal
check_input <- function(data) {
  if (!is.data.frame(data)) {
    stop("Please pass a dataframe to the function")
  }

  # test for dim (vector, one row, one col)
  if (nrow(data) <= 1 || ncol(data) <= 1 || is.null(nrow(data)) || is.null(ncol(data))) {
    stop("Please pass a dataframe to the function with at least 2 columns and rows")
  }
}
