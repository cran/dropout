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

drop_detect <- function(data, last_col = NULL) {
 # Implementation of the function
 result_list <- drop_prepare(data, last_col)
 return(result_list$list_data)
}

