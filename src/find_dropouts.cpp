#include <Rcpp.h>
using namespace Rcpp;
//' @name find_dropouts
 //' @title Identify Dropouts in a Data Frame
 //' @description This function iterates through each row of the provided data frame to identify
 //' the first column from the left where all subsequent columns contain NA values, indicating
 //' a dropout point. It then returns a data frame containing the name of the first dropout column,
 //' a boolean indicating if a dropout was found, and the index of the dropout column for each row.
 //'
 //' @param df A DataFrame object containing the data to be analyzed.
 //' @return A DataFrame containing three columns: \describe{
 //'   \item{dropout_col}{The name of the first column where a dropout occurs. NA if no dropout is found.}
 //'   \item{dropout}{LogicalVector indicating whether a dropout was found in the row.}
 //'   \item{dropout_index}{The index of the dropout column. NA if no dropout is found.}
 //' }
 //' @examples
 //' df <- data.frame(
 //'   x = c(1, NA, 3),
 //'   y = c(4, NA, NA),
 //'   z = c(NA, NA, NA)
 //' )
 //' find_dropouts(df)
 //' @export
 // [[Rcpp::export]]

DataFrame find_dropouts(DataFrame df) {

  int nRows = df.nrows();
  int nCols = df.size();  // Use df.size() instead of non-existent df.ncols()

  CharacterVector df_names = df.attr("names"); // Use df.attr("names") instead of non-existent df.names()

  // Initialize vectors to store the results
  CharacterVector dropout_col(nRows, NA_STRING);
  LogicalVector dropout(nRows, false);
  IntegerVector dropout_index(nRows, NA_INTEGER);

  // Loop through each row
  for(int i = 0; i < nRows; ++i) {

    // Loop through each column
    for(int j = 0; j < nCols; ++j) {

      bool all_na = true;

      // Check if the current cell and all the subsequent cells in the row are NA
      for(int k = j; k < nCols; ++k) {

        if (TYPEOF(df[k]) == INTSXP) {
          IntegerVector col = df[k];
          if (!IntegerVector::is_na(col[i])) {
            all_na = false;
            break;
          }
        } else if (TYPEOF(df[k]) == REALSXP) {
          NumericVector col = df[k];
          if (!NumericVector::is_na(col[i])) {
            all_na = false;
            break;
          }
        } else if (TYPEOF(df[k]) == STRSXP) {
          CharacterVector col = df[k];
          if (!CharacterVector::is_na(col[i])) {
            all_na = false;
            break;
          }
        } else if (TYPEOF(df[k]) == LGLSXP) {
          LogicalVector col = df[k];
          if (!LogicalVector::is_na(col[i])) {
            all_na = false;
            break;
          }
        }

        // Extend this logic for other column types if needed
      }

      // If a dropout column is found, update the result vectors and break the loop
      if(all_na) {
        dropout_col[i] = df_names[j];
        dropout[i] = true;
        dropout_index[i] = j + 1; // R index starts from 1
        break;
      }

    }

  }

  // Create the result data frame
  DataFrame result = DataFrame::create(
    Named("dropout_col") = dropout_col,
    Named("dropout") = dropout,
    Named("dropout_index") = dropout_index
  );

  return result;
}

