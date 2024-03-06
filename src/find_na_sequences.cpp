#include <Rcpp.h>
using namespace Rcpp;
//' @name find_na_sequences
 //' @title Find Sequences of NA Values in a Data Frame
 //' @description This function scans each row of the provided data frame for sequences of NA values
 //' across its columns. For each row, it identifies sequences of NAs that meet or exceed a specified
 //' minimum length (section_min). It returns a list where each element corresponds to a row in the
 //' input data frame and contains vectors of column names that form part of an NA sequence of the
 //' specified minimum length.
 //'
 //' @param sec_data A DataFrame object containing the data to be analyzed.
 //' @param section_min An integer specifying the minimum length of the NA sequences to be identified.
 //' @return A List where each element corresponds to a row in the input data frame. Each element
 //' is a vector of vectors, with each inner vector containing the names of columns that form part of
 //' an NA sequence meeting or exceeding the specified minimum length. Rows without any qualifying NA
 //' sequences will have an empty vector as their corresponding element in the list.
 //' @examples
 //' df <- data.frame(
 //'   A = c(1, NA, 3),
 //'   B = c(NA, NA, 4),
 //'   C = c(NA, NA, NA),
 //'   D = c(4, 5, NA)
 //' )
 //' find_na_sequences(df, 2)
 //' @export
 // [[Rcpp::export]]

List find_na_sequences(DataFrame sec_data, int section_min) {
  int nrows = sec_data.nrows();
  List na_sequences_list(nrows);

  CharacterVector names = sec_data.names();

  for (int row_index = 0; row_index < nrows; ++row_index) {
    std::vector<std::string> current_sequence;
    std::vector<std::vector<std::string>> found_sequences;

    for (int col_index = 0; col_index <= sec_data.size(); ++col_index) {
      bool is_na = false;

      if (col_index < sec_data.size()) {
        SEXP col = sec_data[col_index];
        if (TYPEOF(col) == INTSXP) {
          is_na = (INTEGER(col)[row_index] == NA_INTEGER);
        } else if (TYPEOF(col) == REALSXP) {
          is_na = ISNA(REAL(col)[row_index]);
        } else if (TYPEOF(col) == STRSXP) {
          is_na = (STRING_ELT(col, row_index) == NA_STRING);
        }
        else if (TYPEOF(col) == LGLSXP) {
          is_na = (LOGICAL(col)[row_index] == NA_LOGICAL);
        }
        // Add more types as needed

        if (is_na) {
          current_sequence.push_back(as<std::string>(names[col_index]));
        }
      }

      // Either at the end or encountered a non-NA, consider storing sequence
      if (!is_na || col_index == sec_data.size()) {
        if (current_sequence.size() >= (unsigned)section_min) {
          if (current_sequence.back() != as<std::string>(names[sec_data.size() - 1])) {
            found_sequences.push_back(current_sequence);
          }
        }
        current_sequence.clear();
      }
    }

    if (!found_sequences.empty()) {
      na_sequences_list[row_index] = wrap(found_sequences);
    }
  }

  return na_sequences_list;
}





