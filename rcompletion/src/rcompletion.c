
/* 
   Copyright (C) 2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org> 

   This file is part of the rcompletion package for R.  It is made
   available under the terms of the GNU General Public License,
   version 2, or at your option, any later version, incorporated
   herein by reference.  See the COPYING file for details.

   FYI, note that this file contains code that links to the GNU
   Readline library, which is distributed under the GNU GPL (version 2
   or later), and consequently it's viral clause applies.
*/

/* inspired by fileman.c example in the Readline documentation */



/* #ifdef HAVE_LIBREADLINE

not really necessary since configure will abort otherwise

*/


#include <Rinternals.h>

#ifdef HAVE_READLINE_H
#  include <readline/readline.h>
#else
#  include "readline.h"
#endif

#include "rcompletion.h"


/* Forward declarations */

static void initialize_rlcompletion();
static char **R_custom_completion(const char *text, int start, int end);
static char *R_completion_generator(const char *text, int state);
static char *dupstr(char *s);

/* .Call()-able version that initiates our custom binding */

SEXP RCompletionInit()
{
    initialize_rlcompletion();
    return R_NilValue;
}

/* SEXP RCSuppressFileCompletion() */
/* { */
/*     rl_attempted_completion_over = 1; */
/*     return R_NilValue; */
/* } */


/* 
   A note on environments: tentative completions are calculated using
   R code, and most of the code in this file is just a communication
   layer between R and readline.  For every completion attempt, R code
   has to be called for 3 distinct purposes (this is the way we do it,
   not the way it _has_ to be done): (1) make available to R
   information that it needs to compute possible completions, (2)
   evaluate the completions and (3) retrieve the completions.  These
   are done by constructing R function calls using the lang1 and lang2
   interfaces which are then evaluated using eval.  eval needs an
   environment.  This is where things get slightly tricky.

   Since these R functions are not of any use to the user, they are
   not exported.  This means that they won't be visible from
   R_GlobalEnv.  So, they should really be evaluated in the package
   environment.  Normally, that environment could have been passed as
   an argument from R in a .Call.  However, we are in the funny
   situation where the C code is called from a readline binding, not
   via .Call, so that solution won't work.

   What we use now is 

	rho = PROTECT(R_FindNamespace(mkString("rcompletion"))),

   See r-devel thread at
   https://stat.ethz.ch/pipermail/r-devel/2006-October/039884.html
   for details.

*/





/* Tell the GNU Readline library how to complete. */
static void initialize_rlcompletion ()
{
  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = "RCustomCompletion";

  /* Tell the completer that we want a crack first. */
  rl_attempted_completion_function = R_custom_completion;

  /* Don't want spaces appended at the end */
  rl_completion_append_character = '\0';

  /* token boundaries.  Includes *,+ etc, but not $,@ because those
     are easier to handle at the R level if the whole thing is
     available.  However, this breaks filename completion if partial
     filenames contain things like $, % etc.  Might be possible to
     associate a M-/ override like bash does.  One compromise is that
     we exclude / from the breakers because that is frequently found
     in filenames even though it is also an operator.  This can be
     handled in R code (although it shouldn't be necessary if users
     surround operators with spaces, as they should).  */

  /* FIXME: quotes currently lead to filename completion without any
     further ado.  This is not necessarily the best we can do, since
     quotes after a [, $, [[, etc should be treated differently.  I'm
     not testing this now, but this should be doable by removing quote
     characters from the strings below and handle it with other things
     in 'specialCompletions()' in R.  The problem with that approach
     is that file name completion will probably have to be done
     manually in R, which is not trivial.  One way to go might be to
     forego file name completion altogether when TAB completing, and
     associate M-/ or something to filename completion (a startup
     message might say so, to remind users)

     All that might not be worth the pain though (vector names would
     be practically impossible, to begin with) */


  /* Not sure why the second one is needed */
  rl_basic_word_break_characters = " \t\n\\\"'`><=+-*%;,|&{()}[]";
  rl_completer_word_break_characters = " \t\n\\\"'`><=+-*%;,|&{()}";
  return;
}



/* Attempt to complete on the contents of TEXT.  START and END bound the
   region of rl_line_buffer that contains the word to complete.  TEXT is
   the word to complete.  We can use the entire contents of rl_line_buffer
   in case we want to do some simple parsing.  Return the array of matches,
   or NULL if there aren't any. */



char **
R_custom_completion(const char *text, 
		    int start, 
		    int end)
     /* 
	Make some relevant information available to R, then call
	rl_completion_matches to generate matches.  FIXME: It would be
	nice if we could figure whether we are in a partially
	completed line (R prompt == "+"), in which case we could keep
	the old line buffer around and do useful things with it.
     */
{
    char **matches = (char **)NULL;
    SEXP 
	infile,
	rho            = PROTECT(R_FindNamespace(mkString("rcompgen"))),
	linebufferCall = PROTECT(lang2(RComp_assignBufferSym,  mkString(rl_line_buffer))), 
	startCall      = PROTECT(lang2(RComp_assignStartSym,   ScalarInteger(start))),
	endCall        = PROTECT(lang2(RComp_assignEndSym,     ScalarInteger(end)));
    eval(linebufferCall, rho);
    eval(startCall, rho);
    eval(endCall, rho);
    UNPROTECT(4);

    matches = rl_completion_matches(text, R_completion_generator);

    infile = PROTECT(eval(lang1(RComp_getFileCompSym), rho));
    if (!asLogical(infile)) rl_attempted_completion_over = 1;
    UNPROTECT(1);

    return (matches);
}





/* R_completion_generator does the actual work (it is called from
   somewhere inside rl_completion_matches repeatedly).  See readline
   documentation for details, but one important fact is that the
   return value of R_completion_generator will be free()-d by
   readline, so that memory management has to be separated from R's (I
   think).  FIXME: should be more defensive when malloc()-ing (also,
   what's xmalloc()?) */


/* Generator function for command completion.  STATE lets us know
   whether to start from scratch: we do so when STATE == 0 */

char *
R_completion_generator(const char *text, int state)
{
    static int list_index, ncomp;
    static char **compstrings;

    /* If this is a new word to complete, initialize now.  This
       involves saving 'text' to somewhere R can get at it, calling
       completeToken(), and retrieving the completions. */

    if (!state) {
	int i;
	SEXP 
	    completions,
	    rho            = PROTECT(R_FindNamespace(mkString("rcompgen"))),
	    assignCall     = PROTECT(lang2(RComp_assignTokenSym, mkString(text))), 
	    completionCall = PROTECT(lang1(RComp_completeTokenSym)), 
	    retrieveCall   = PROTECT(lang1(RComp_retrieveCompsSym));

	eval(assignCall, rho);
	eval(completionCall, rho);
	PROTECT(completions = eval(retrieveCall, rho));

	list_index = 0;
	ncomp = length(completions);

	if (ncomp > 0) {
	    compstrings = (char **) malloc(ncomp * sizeof(char*));
	    for (i = 0; i < ncomp; i++)
		compstrings[i] = dupstr(CHAR(STRING_ELT(completions, i)));
	}
	UNPROTECT(5);
    }

    /* Return next */

    if (list_index < ncomp) {
	return compstrings[list_index++];
    }
    else {
	/* nothing matched or remaining, so return NULL. */ 
	if (ncomp > 0) free(compstrings);
	return ((char *)NULL);
    }
}

/* Utility to make a copy of a string */

char *dupstr(char *s)
{
    char *r;
    r = (char *) malloc((strlen (s) + 1) * sizeof(char));
    strcpy (r, s);
    return (r);
}




/* #endif */






