#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP drop_s(SEXP list_of_answers) {

 int num_answers = Rf_length(list_of_answers);

 // Create a list to store the results for each answer vector
 SEXP result_list = PROTECT(Rf_allocVector(VECSXP, num_answers));

 for (int j = 0; j < num_answers; ++j) {
  SEXP answers = VECTOR_ELT(list_of_answers, j);
  int len = Rf_length(answers);
  int* ans = INTEGER(answers);

  // Initialize vectors to store results
  SEXP dropout = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP sequence_length = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP sequence_na = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP single_na = PROTECT(Rf_allocVector(INTSXP, len));

  // Initialize all values to 0
  for (int i = 0; i < len; ++i) {
   INTEGER(dropout)[i] = 0;
   INTEGER(sequence_length)[i] = 0;
   INTEGER(sequence_na)[i] = 0;
   INTEGER(single_na)[i] = 0;
  }

  int na_counter = 0;

  for (int i = 0; i < len; ++i) {
   if (na_counter == 0) {
    if (ans[i] == 1) {
     if (i == len - 1) {
      INTEGER(dropout)[i] = 1;
     } else if (ans[i] == 1) {
      na_counter += 1;
     }
    }
   } else {
    if (ans[i] == 0) {
     if (na_counter > 1) {
      INTEGER(sequence_length)[i - na_counter] = na_counter;
      for (int k = (i - na_counter); k < i; ++k) {
       INTEGER(sequence_na)[k] = INTEGER(sequence_na)[k] + 1;
      }
     } else {
      INTEGER(single_na)[i - 1] = 1;
     }
     na_counter = 0;
    } else if (ans[i] == 1 && i == len - 1) {
     INTEGER(dropout)[i - na_counter] = 1;
    } else {
     na_counter += 1;
    }
   }
  }

  // Create a list to return all results for this answer vector
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(result, 0, dropout);
  SET_VECTOR_ELT(result, 1, sequence_length);
  SET_VECTOR_ELT(result, 2, sequence_na);
  SET_VECTOR_ELT(result, 3, single_na);

  // Store the result in the result list
  SET_VECTOR_ELT(result_list, j, result);

  // Unprotect the vectors created in this iteration
  UNPROTECT(5);
 }

 // Unprotect the result list and return it
 UNPROTECT(1);
 return result_list;
}






SEXP drop_d(SEXP list_of_answers) {

 int num_answers = Rf_length(list_of_answers);

 // Create a list to store drop_bool and drop_index
 SEXP result_list = PROTECT(Rf_allocVector(VECSXP, 3));

 SEXP dropout_bool = PROTECT(Rf_allocVector(INTSXP, num_answers));
 SEXP dropout_index = PROTECT(Rf_allocVector(INTSXP, num_answers));

 // Initialize all values to 0
 for (int i = 0; i < num_answers; ++i) {
  INTEGER(dropout_bool)[i] = 0;
  INTEGER(dropout_index)[i] = 0;
 }

 for (int j = 0; j < num_answers; ++j) {
  SEXP answers = VECTOR_ELT(list_of_answers, j);
  int len = Rf_length(answers);
  int* ans = INTEGER(answers);

  int na_counter = 0;

  for (int i = 0; i < len; ++i) {
   if (na_counter == 0 && ans[i] == 1 && i == len - 1) {
    INTEGER(dropout_bool)[j] = 1;
    INTEGER(dropout_index)[j] = i + 1;
   } else if (na_counter > 0 && ans[i] == 1 && i == len - 1) {
    INTEGER(dropout_bool)[j] = 1;
    INTEGER(dropout_index)[j] = i - na_counter + 1;
   } else if (ans[i] == 0) {
    na_counter = 0;
   } else if (ans[i] == 1) {
    na_counter += 1;
   }
  }
 }

 SET_VECTOR_ELT(result_list, 0, dropout_bool);
 SET_VECTOR_ELT(result_list, 1, dropout_index);

 // Unprotect the result list and return it
 UNPROTECT(3);
 return result_list;
}

