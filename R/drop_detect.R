#' Detecting Dropouts in Surveys
#'
#' `drop_detect` function detects participants who have dropped out of a survey.
#' It identifies sequences of NA values up to the last survey question and pinpoints the column where the dropout occurred.
#' It also provides the index of that column, enabling targeted analysis of dropout patterns.
#'
#' @param data A dataframe or tibble containing the survey data.
#' @param last_col The index position or column name of the last survey item. This is optional and is used when there are additional columns in the data frame that are not part of the survey questions you are interested in.
#'
#' @return A dataframe or tibble where each row corresponds to a row in the original dataset. It contains three columns:
#' - `dropout`: A logical indicating whether a dropout was detected for this row.
#' - `dropout_column`: If `dropout` is TRUE, the name of the column where the dropout occurred.
#' - `dropout_index`: If `dropout` is TRUE, the index (column number) where the dropout occurred.
#'
#' @examples
#' # Basic usage
#' drop_detect(flying, "location_census_region")
#'
#' @seealso See vignette for detailed workflows and practical examples.
#'
#' @export
#'
#' @useDynLib dropout
#' @importFrom Rcpp sourceCpp



drop_detect <- function(data, last_col = NULL) {

 if (is.null(last_col)) {
  while (ncol(data) > 0) {
   last_col <- ncol(data)
   last_col_name <- colnames(data)[last_col]

   if (all(!is.na(data[, last_col]))) {
    data <- data |> dplyr::select(-last_col)
   } else {
    warning(paste("last_col set to", last_col_name))
    break
   }
  }
 } else {
  # Select all columns up to last_col
  data <- data |> dplyr::select(1:last_col)
 }

 result <- data |>
  find_dropouts() |>
  tibble::tibble()

 return(result)
}

