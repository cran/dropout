#' Summarize Missing Data Metrics for Each Column
#'
#' The `drop_summary` function generates a summary of missing data (NA values) for each column in a dataframe.
#' It computes various metrics such as the number of dropout participants, section NAs, the mode length of those missing value sections for, and the proportion of complete cases for each column.
#'
#' The function calls a C API to compute some metrics, which are then processed and returned as a summary dataframe.
#'
#' @param data A dataframe for which to analyze missing data.
#'
#' @return A dataframe containing the following columns:
#' \itemize{
#'   \item \strong{column}: The name of each column in the input dataframe.
#'   \item \strong{drop}: The number of dropped rows (missing values) for that column.
#'   \item \strong{sec_na}: The number of sections of consecutive NAs for that column.
#'   \item \strong{sec_length}: The mode (most frequent length) of sections of consecutive NAs for that column.
#'   \item \strong{single_na}: The number of single NA values (isolated missing values) for that column.
#'   \item \strong{na}: The total number of missing (NA) values for that column.
#'   \item \strong{complete}: The proportion of complete rows for that column, where a value of 1 means no missing data, and values closer to 0 mean more missing data.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with the 'flying' dataframe
#' summary_result <- drop_summary(flying)
#' print(summary_result)
#' }
#'
#' @export
drop_summary <- function(data) {
 c_output <- .Call("drop_s", c_prepare(data))
 out <- metrics_summary(data, c_output)
 return(out)
}

