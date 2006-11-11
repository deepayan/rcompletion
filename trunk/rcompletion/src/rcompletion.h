
#ifndef RL_COMPLETION_H
#define RL_COMPLETION_H

SEXP RCompletionInit();
SEXP RCSuppressFileCompletion();

SEXP 
    RComp_assignBufferSym,
    RComp_assignStartSym,
    RComp_assignEndSym,
    RComp_assignTokenSym,
    RComp_completeTokenSym,
    RComp_retrieveCompsSym;

#endif

