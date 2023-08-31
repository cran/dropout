#' internal function to detect dropouts
#' @param data dataframe or tibble to detect dropouts.
#' @param last_col Index position or column name of the last survey item. This is an optional argument and is needed if there are other columns in your dataframe after the survey items you want to detect the dropout for.
#' @return A list containing a dataframe or tibble with the following for each row of your dataset: A logical whether a dropout has been detected and, if so, the column name where it occurs and the index of the column. As well as the value of the col_index


  drop_prepare <- function(data, last_col) {
    # Check if there is the optional operator last col, otherwise use data
    if (missing(last_col)) {
      data
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

      data <- data[, 1:col_index]

    }

    data_subset <-
      output <- apply(data, 1, function(row) {
        dropout_col <- character(1)
        for (i in 1:(ncol(data))) {
          if (all(is.na(row[i:ncol(data)]))) {
            dropout_col <- colnames(data)[i]
            break
          }
        }
        if (identical(dropout_col, character(0))) {
          dropout_col <- NA
        }
        return(dropout_col)
      })

    output <- ifelse(output == "", NA, output)

    drop_data <- data.frame(dropout_column = output)

    if (!any(is.na(data[, ncol(data)]))) {
      warning("No Dropouts detected. \n Please set last_col to the the last survey item. Use ?drop_detect to find out more")
    }

    drop_data$dropout <- !is.na(drop_data$dropout_column)

    # experimental (dropout index)

    drop_data$dropout_index <- ifelse(!is.na(drop_data$dropout_column), match(drop_data$dropout_column, colnames(data)), NA)

    if (requireNamespace("tibble", quietly = TRUE)) {
      drop_data <- tryCatch(
        tibble::as_tibble(drop_data),
        error = function(e) {
          drop_data$dropout <- as.logical(drop_data$dropout)
          drop_data
        }
      )
    }

    if (missing(last_col)){
      return(list(list_data = drop_data, list_index = ncol(data)))
    } else {
      return(list(list_data = drop_data, list_index = col_index))
    }

  }
