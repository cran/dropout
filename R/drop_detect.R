#' Detect Instances of Dropout in Data
#'
#' The `drop_detect` function detects participants who drop out of the survey by recognizing NA sequences up to the last question of the survey. Additionally, the function provides the column name and index where the dropout occurs.
#'
#' @param data A dataframe in which to detect instances of dropout.
#'
#' @return A dataframe containing the following columns:
#' \itemize{
#'   \item \strong{drop}: A logical value indicating whether dropout has occurred (`TRUE` for dropout, `FALSE` otherwise).
#'   \item \strong{drop_index}: The index of the column where dropout occurred (`NA` if no dropout).
#'   \item \strong{column}: The name of the column where the dropout occurred (`<NA>` if no dropout).
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with the 'flying' dataframe
#' detect_result <- drop_detect(flying)
#' print(detect_result)
#' }
#'
#' @export
drop_detect <- function(data) {
 c_output <- .Call("drop_d", c_prepare(data))
 out <- metrics_detect(data, c_output)
 return(out)
}

