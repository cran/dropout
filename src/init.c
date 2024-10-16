#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/* .Call calls */
extern SEXP drop_s(SEXP);
extern SEXP drop_d(SEXP);

static const R_CallMethodDef CallEntries[] = {
 {"addr_drop_s", (DL_FUNC) &drop_s, 1},
 {"addr_drop_d", (DL_FUNC) &drop_d, 1},
 {NULL, NULL, 0}
};

void R_init_addr(DllInfo *dll) {
 R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
 R_useDynamicSymbols(dll, FALSE);
}
