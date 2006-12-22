

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "rcompletion.h"

static R_CallMethodDef CallEntries[] = {
    {"RCompletionInit",          (DL_FUNC) &RCompletionInit,          0},
/*  {"RCSuppressFileCompletion", (DL_FUNC) &RCSuppressFileCompletion, 0}, */
    {NULL, NULL, 0}
};

void R_init_rcompletion(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    /* The following are in the rcompgen package */
    RComp_assignBufferSym  = install(".assignLinebuffer");
    RComp_assignStartSym   = install(".assignStart");
    RComp_assignEndSym     = install(".assignEnd");
    RComp_assignTokenSym   = install(".assignToken");
    RComp_completeTokenSym = install(".completeToken");
    RComp_getFileCompSym   = install(".getFileComp");
    RComp_retrieveCompsSym = install(".retrieveCompletions");
}

