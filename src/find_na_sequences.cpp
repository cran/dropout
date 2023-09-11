#include <Rcpp.h>
using namespace Rcpp;

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





