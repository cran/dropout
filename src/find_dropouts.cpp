#include <Rcpp.h>
using namespace Rcpp;

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

