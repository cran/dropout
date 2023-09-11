#' Summarizing Dropouts in Surveys
#'
#' `drop_summary` function provides a high-level summary of dropout occurrences in the survey data.
#' It generates key statistics to understand the patterns of participant dropouts across different survey questions.
#'
#' @param data A dataframe or tibble containing the survey data.
#' @param last_col The index position or column name of the last survey item. This is optional and is used when there are additional columns in the data frame that are not part of the survey questions you are interested in.
#' @param section_min Indicates occurrences of missing values that span at least n consecutive columns (n defaults to 3)
#'
#' @return A dataframe or tibble containing summary statistics about dropouts. Typical columns might include:
#' - `question_name`: The name of the survey question or column.
#' - `dropout_count`: The number of dropouts at this question.
#' - `dropout_percentage`: The percentage of participants who dropped out at this question.
#'
#' @examples
#' # Basic usage
#' drop_summary(flying, "location_census_region")
#'
#' # Summarizing dropouts up to a specific column
#' drop_summary(flying, last_col = "age")
#'
#' # Read more in the vignette for interpreting summary statistics and plotting dropout trends.
#'
#' @seealso See vignette for detailed workflows, tips on interpretation, and practical examples.
#'
#' @export





drop_summary <- function(data, last_col = NULL, section_min = 3) {

  if (is.null(last_col)) {
    while (ncol(data) > 0) {
      last_col <- ncol(data)
      last_col_name <- colnames(data)[last_col]

      if (all(!is.na(data[, last_col]))) {
        data <- data |>
          dplyr::select(-last_col)
      } else {
        warning(paste("last_col set to", last_col_name))
        break
      }
    }
  } else {
    # Select all columns up to last_col
    data <- data |>
      dplyr::select(1:last_col)
  }

  result <- data |>
    find_dropouts()

  # save result in a list (df and index)
  drop_df <- list(list_data = result,
                  list_index =
                    if(is.numeric(last_col)){
                      last_col
                    } else {
                      which(names(data) == last_col)
                    })

  # Get the column index of the last column and the dropout columns
  col_index <- drop_df$list_index
  drop_df <- drop_df$list_data[-c(2,3)]

  # Maintaining original column order through factor levels
  drop_df$dropout_column <- factor(drop_df$dropout_col, levels = names(data[, 1:col_index]))

  # Group by 'dropout_column' and count occurrences
  grouped_counts <- table(drop_df$dropout_column)

  # Create a new data frame with the counts
  result_df <- data.frame(column_name = names(grouped_counts), dropout = as.vector(grouped_counts))

  # Compute drop rates and other metrics
  result_df$drop_rate <- round(cumsum(result_df$dropout) / nrow(data), 2)
  result_df$missing <- colSums(is.na(data[, 1:col_index]))
  result_df$completion_rate <- round((nrow(data) - result_df$missing) / nrow(data), 2)


  # performing drop_prepare sub sample also for section_dropout
  if (missing(last_col)) {
    sec_data <- data
  } else {
    if (is.character(last_col)) {
      col_index <- which(names(data) == last_col)
    } else if (is.numeric(last_col)) {
      col_index <- last_col
    } else {
      stop("Column must be specified by name or index.")
    }

    if (length(col_index) != 1) {
      stop("Column not found or ambiguous.")
    }

    sec_data <- data[, 1:col_index]
  }
  # Initialize variables for section dropout calculations
  result <- list()

  result <- find_na_sequences(sec_data, section_min)

  # Count how often each column appears in the sequences
  column_counts <- data.frame(table(unlist(result)))

  # Check if column_counts has any rows and columns, otherwise initialize it
  if (nrow(column_counts) == 0) {
    column_counts <- data.frame(column_name = character(0), section_na = integer(0))
  } else if (ncol(column_counts) == 1) {
    column_counts$section_na <- 0  # Or some other default value
  }

  names(column_counts) <- c("column_name", "section_na")

  # Merge result_df with column_counts, ensuring original order is maintained
  matched_indices <- match(result_df$column_name, column_counts$column_name)

  # Preserving the order of 'result_df' and adding new columns
  result_df$section_na <- column_counts$section_na[matched_indices]

  # If there might be NAs in the matched indices, then replace NAs in the new columns with 0
  result_df[is.na(result_df)] <- 0

  # Compute single_na and drop_na metrics
  result_df$single_na <- result_df$missing - cumsum(result_df$dropout) - result_df$section_na
  result_df$drop_na <- result_df$missing - result_df$single_na - result_df$section_na

  # Reorder the columns
  result_df <- result_df[, c("column_name", "dropout", "drop_rate", "drop_na", "section_na", "single_na", "missing", "completion_rate")]

  # Reorder the data frame based on the original column order
  original_order <- match(result_df$column_name, names(data[, 1:col_index]))
  result_df <- result_df[order(original_order), ]

  return(tibble::tibble(result_df))
}


