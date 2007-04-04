
### Copyright (C) 2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the rcompletion package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA





### Note: try not to do things that might be slow due to network
### latency (think NFS).  For example, retrieving a list of available
### packages is potentially slow for this reason.


### Status: I'm mostly happy with things.  The only obvious
### improvement I can think of is figuring out when we are in
### continuation mode (R prompt == "+") and make use of previous lines
### in that case.  I haven't found a way to do that.



### Note: sprintf seems faster than paste based on naive benchmarking:

## > system.time(for (i in 1:100000) sprintf("foo%sbar%d", letters[1:26], 1:26) )
##            user          system           total   user.children system.children
##           4.796           0.088           4.887           0.000           0.000
## > system.time(for (i in 1:100000) paste("foo", letters[1:26], "bar", 1:26) )
##            user          system           total   user.children system.children
##           8.300           0.028           8.336           0.000           0.000

### so will change all pastes to sprintf.  However, we need to be
### careful because 0 length components in sprintf will cause errors.



## modifies settings:

rc.settings <- function(ops, ns, args, func, ipck, S3, data, help, argdb)
{
    checkAndChange <- function(what, value)
    {
        if ((length(value) == 1) &&
            is.logical(value) &&
            !is.na(value))
            .CompletionEnv$settings[[what]] <- value
    }
    if (!missing(ops))   checkAndChange( "ops",  ops)
    if (!missing(ns))    checkAndChange(  "ns",   ns)
    if (!missing(args))  checkAndChange("args", args)
    if (!missing(func))  checkAndChange("func", args)
    if (!missing(ipck))  checkAndChange("ipck", ipck)
    if (!missing(S3))    checkAndChange("S3", S3)
    if (!missing(data))  checkAndChange("data", S3)
    if (!missing(help))  checkAndChange("help", S3)
    if (!missing(argdb)) checkAndChange("argdb", S3)
    invisible()
}





## modifies options (adapted from similar functions in lattice):

rc.getOption <- function(name)
{
    get("options", envir = .CompletionEnv)[[name]]
}

rc.options <- function(...)
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]]))
        new <- new[[1]]
    old <- .CompletionEnv$options

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)
    ## typically getting options
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones
    ## should be set.  Everything should be returned though.

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .CompletionEnv$options <- modifyList(old, new[nm])
    invisible(retVal)
}



## summarizes results of last completion attempt:

rc.status <- function()
{
    ## eapply(.CompletionEnv, function(x) x, all.names = TRUE)
    as.list(.CompletionEnv)
}


### Everything below is unexported


## accessors called from C (also .completeToken below):

.assignToken         <- function(text)  assign("token",      text,  envir = .CompletionEnv)
.assignLinebuffer    <- function(line)  assign("linebuffer", line,  envir = .CompletionEnv)
.assignStart         <- function(start) assign("start",      start, envir = .CompletionEnv)
.assignEnd           <- function(end)   assign("end",        end,   envir = .CompletionEnv)
.setFileComp         <- function(state) assign("fileName",   state, envir = .CompletionEnv)

.retrieveCompletions <- function()         get("comps",             envir = .CompletionEnv)
.getFileComp         <- function()         get("fileName",          envir = .CompletionEnv)




## The following function is not required for GNU readline, but can be
## useful if one needs to break a line into tokens.  It requires
## linebuffer and end to be already set, and itself sets token and
## start.  It returns the token.


.guessTokenFromLine <-
    function()
{
    linebuffer <- .CompletionEnv[["linebuffer"]]
    end <- .CompletionEnv[["end"]]
    start <- suppressWarnings(gregexpr("[^\\.\\w:?$@[\\]]+", substr(linebuffer, 1, end), perl = TRUE))[[1]]
    ##                                    ^^^^^^^^^^^^^^
    ##                             things that should not cause breaks
    start <- ## 0-indexed
        if (all(start < 0)) 0
        else tail(start + attr(start, "match.length"), 1) - 1 
    .CompletionEnv[["start"]] <- start
    .CompletionEnv[["token"]] <- substr(linebuffer, start + 1, end)
    .CompletionEnv[["token"]]
}






## convert a string to something that escapes special regexp
## characters.  Doesn't have to be perfect, especially for characters
## that would cause breaks or be handled elsewhere.  All we really
## need is to handle ".", so that e.g. "heat." doesn't match
## "heatmap".

makeRegexpSafe <- function(s)
{
    s <- gsub(".", "\\.", s, fixed = TRUE)
    ## what else?
    s
}



## Operators that are handled specially.  Order is important, ::: must
## come before :: (because :: will match :::)

specialOps <- c("$", "@", ":::", "::", "?", "[", "[[")



specialOpLocs <- function(text)
{
    ## does text contain a special operator?  There may be multiple
    ## occurrences, and we want the last one (whereas regexpr gives
    ## the first). So...

    ge <-
        sapply(specialOps,
               function(s) gregexpr(s, text, fixed = TRUE)[[1]],
               simplify = FALSE)
    ## this gets the last ones
    ge <- sapply(ge, tail, 1)
    ge <- ge[ge > 0]
}



## accessing the help system: should allow anything with an index entry

matchAvailableTopics <-
    function(text)
{
    if (length(text) != 1 || text == "") return (character(0))
    ll <- installed.packages()[.packages(), "LibPath"]
    indexFiles <- file.path(ll, names(ll), "help", "AnIndex")
    unique(unlist(lapply(indexFiles,
                         function(f) {
                             foo <-
                                 scan(f, what = list("", ""),
                                      sep = "\t",
                                      quote = "",
                                      na.strings = "",
                                      quiet = TRUE)[[1]]
                             grep(sprintf("^%s", makeRegexpSafe(text)),
                                  foo, value = TRUE)
                         })))
}




## this is for requests of the form ?suffix[TAB] or prefix?suffix[TAB]

## can be improved when prefix is non-trivial, but that is rarely used
## (on the other hand, can be useful for exploring S4 methods; but
## usage of ? needs to be fixed in R first).  Anyway, that case is not
## currently handled

helpCompletions <- function(prefix, suffix)
{
    nc <-
        if (.CompletionEnv$settings[["help"]])
            matchAvailableTopics(suffix)
        else
            normalCompletions(suffix, check.mode = FALSE)
    if (length(nc) > 0) sprintf("%s?%s", prefix, nc)
    else character(0)
}


specialCompletions <- function(text, spl)
{

    ## we'll only try to complete after the last special operator, and
    ## assume that everything before is meaningfully complete.  A more
    ## sophisticated version of this function may decide to do things
    ## differently.

    ## Note that this will involve evaluations, which may have side
    ## effects.  This (side-effects) would not happen normally (except
    ## of lazy loaded symbols, which most likely would have been
    ## evaluated shortly anyway), because explicit function calls
    ## (with parentheses) are not evaluated.  In any case, these
    ## evaluations won't happen if settings$ops==FALSE 

    ## spl (locations of matches) is guaranteed to be non-empty

    wm <- which.max(spl)
    op <- names(spl)[wm]
    opStart <- spl[wm]
    opEnd <- opStart + nchar(op)

    if (opStart < 1) return(character(0)) # shouldn't happen
    prefix <- substr(text, 1, opStart - 1)
    suffix <- substr(text, opEnd, 1e6)

    if (op == "?") return(helpCompletions(prefix, suffix))

    if (opStart <= 1) return(character(0)) # not meaningful

    ## ( breaks words, so prefix should not involve function calls,
    ## and thus, hopefully no side-effects.

    tryToEval <- function(s)
    {
        try(eval(parse(text = s), envir = .GlobalEnv), silent = TRUE)
    }

    comps <-
        switch(op,
               "$" = {
                   if (.CompletionEnv$settings[["ops"]])
                   {
                       object <- tryToEval(prefix)
                       if (inherits(object, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           ## suffix must match names(object) (or ls(object) for environments)
                           if (is.environment(object))
                           {
                               ls(object,
                                  all.names = TRUE,
                                  pattern = sprintf("^%s", makeRegexpSafe(suffix)))
                           }
                           else
                           {
                               grep(sprintf("^%s", makeRegexpSafe(suffix)),
                                    names(object), value = TRUE)
                           }
                       }
                   } else suffix
               },
               "@" = {
                   if (.CompletionEnv$settings[["ops"]])
                   {
                       object <- tryToEval(prefix)
                       if (inherits(object, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           grep(sprintf("^%s", makeRegexpSafe(suffix)),
                                methods::slotNames(object), value = TRUE)
                       }
                   } else suffix
               },
               "::" = {
                   if (.CompletionEnv$settings[["ns"]])
                   {
                       nse <- try(getNamespaceExports(prefix), silent = TRUE)
                       if (inherits(nse, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           grep(sprintf("^%s", makeRegexpSafe(suffix)),
                                nse, value = TRUE)
                       }
                   } else suffix
               },
               ":::" = {
                   if (.CompletionEnv$settings[["ns"]])
                   {
                       ns <- try(getNamespace(prefix), silent = TRUE)
                       if (inherits(ns, "try-error")) ## nothing else to do
                           suffix
                       else
                       {
                           ls(ns,
                              all.names = TRUE,
                              pattern = sprintf("^%s", makeRegexpSafe(suffix)))
                       }
                   } else suffix
               },
               "[" = ,  # can't think of anything else to do
               "[[" = {
                   comps <- normalCompletions(suffix)
                   if (length(comps) > 0) comps
                   else suffix
               })
    if (length(comps) == 0) comps <- ""
    sprintf("%s%s%s", prefix, op, comps)
}



## completions on special keywords (subset of those in gram.c).  Some
## issues with parentheses: e.g. mode(get("repeat")) is "function", so
## it is normally completed with a left-paren appended, but that is
## not normal usage.  Putting it here means that both 'repeat' and
## 'repeat(' will be valid completions (as they should be)


keywordCompletions <- function(text)
{
    grep(sprintf("^%s", makeRegexpSafe(text)),
         c("NULL", "NA", "TRUE", "FALSE", "GLOBAL.ENV", "Inf", "NaN",
           "repeat ", "in ", "next ", "break "),
         value = TRUE)
}

## 'package' environments in the search path.  These will be completed
## with a :: IIRC, that works even if there's no namespace, but I
## haven't actually checked.



attachedPackageCompletions <- function(text, add = rc.getOption("package.suffix"))
{
    if (.CompletionEnv$settings[["ns"]])
    {
        s <- grep("^package", search(), value = TRUE)
        comps <- 
            grep(sprintf("^%s", makeRegexpSafe(text)),
                 substr(s, 9, 1e6),
                 value = TRUE)
        if (length(comps) > 0 && !is.null(add))
            sprintf("%s%s", comps, add)
        else
            comps
    }
    else character(0)
}



## this provides the most basic completion, looking for completions in
## the search path using apropos, plus keywords.  Plus completion on
## attached packages if settings$ns == TRUE


normalCompletions <-
    function(text, check.mode = TRUE,
             add.fun = rc.getOption("function.suffix"))
{
    ## use apropos or equivalent
    if (text == "") character() ## too many otherwise
    else
    {
        comps <- apropos(sprintf("^%s", makeRegexpSafe(text)), ignore.case = FALSE)
        if (.CompletionEnv$settings[["func"]] && check.mode && !is.null(add.fun))
        {
            which.function <- sapply(comps, function(s) exists(s, mode = "function"))
            if (any(which.function))
                comps[which.function] <-
                    sprintf("%s%s", comps[which.function], add.fun)
            ##sprintf("\033[31m%s\033[0m%s", comps[which.function], add.fun)
        }
        c(comps, keywordCompletions(text), attachedPackageCompletions(text))
    }
}


## completion on function arguments.  This involves the most work (as
## we need to look back in the line buffer to figure out which
## function we are inside, if any), and is also potentially intensive
## when many functions match the function that we are supposedly in
## (which will happen for common generic functions like print (we are
## very optimistic here, erring on the side of
## whatever-the-opposite-of-caution-is (our justification being that
## erring on the side of caution is practically useless and not erring
## at all is expensive to the point of being impossible (we really
## don't want to evaluate the dotlot() call in "print(dotplot(x),
## positi[TAB] )" ))))


## this defines potential function name boundaries

breakRE <- "[^\\.\\w]"
## breakRE <- "[ \t\n \\\" '`><=-%;,&}\\\?\\\+\\\{\\\|\\\(\\\)\\\*]"




## for some special functions like library, data, etc, normal
## completion is rarely meaningful, especially for the first argument.
## Unfortunately, knowing whether the token being completed is the
## first arg of such a function involves more work than we would
## normally want to do.  On the other hand, inFunction() below already
## does most of this work, so we will add a piece of code (mostly
## irrelevant to its primary purpose) to indicate this.  The following
## two functions are just wrappers to access and modify this
## information.


setIsFirstArg <- function(v)
    .CompletionEnv[["isFirstArg"]] <- v

getIsFirstArg <- function()
    .CompletionEnv[["isFirstArg"]]



inFunction <-
    function(line = .CompletionEnv[["linebuffer"]],
             cursor = .CompletionEnv[["start"]])
{
    ## are we inside a function? Yes if the number of ( encountered
    ## going backwards exceeds number of ).  In that case, we would
    ## also like to know what function we are currently inside
    ## (ideally, also what arguments to it have already been supplied,
    ## but let's not dream that far ahead).

    parens <-
        sapply(c("(", ")"),
               function(s) gregexpr(s, substr(line, 1, cursor), fixed = TRUE)[[1]],
               simplify = FALSE)
    ## remove -1's
    parens <- lapply(parens, function(x) x[x > 0])

    ## The naive algo is as follows: set counter = 0; go backwards
    ## from cursor, set counter-- when a ) is encountered, and
    ## counter++ when a ( is encountered.  We are inside a function
    ## that starts at the first ( with counter > 0.

    temp <-
        data.frame(i = c(parens[["("]], parens[[")"]]),
                   c = rep(c(1, -1), sapply(parens, length)))
    if (nrow(temp) == 0) return(character(0))
    temp <- temp[order(-temp$i), , drop = FALSE] ## order backwards
    wp <- which(cumsum(temp$c) > 0)
    if (length(wp) > 0) # inside a function
    {
        ## return guessed name of function, letting someone else
        ## decide what to do with that name

        index <- temp$i[wp[1]]
        prefix <- substr(line, 1, index - 1)
        suffix <- substr(line, index + 1, cursor + 1)



        ## note in passing whether we are the first argument (no '='
        ## and no ',' in suffix)

        if ((length(grep("=", suffix, fixed = TRUE)) == 0) &&
            (length(grep(",", suffix, fixed = TRUE)) == 0))
            setIsFirstArg(TRUE)


        if ((length(grep("=", suffix, fixed = TRUE)) > 0) &&
            (length(grep(",", substr(suffix,
                                     tail(gregexpr("=", suffix, fixed = TRUE)[[1]], 1),
                                     1e6), fixed = TRUE)) == 0))
        {
            ## we are on the wrong side of a = to be an argument
            return(character(0))
        }
        else ## guess function name
        {
            possible <- suppressWarnings(strsplit(prefix, breakRE, perl = TRUE))[[1]]
            possible <- possible[possible != ""]
            if (length(possible) > 0) return(tail(possible, 1))
            else return(character(0))
        }
    }
    else # not inside function
    {
        return(character(0))
    }
}


argNames <-
    function(fname, use.arg.db = .CompletionEnv$settings[["argdb"]])
{
    if (use.arg.db) args <- .FunArgEnv[[fname]]
    if (!is.null(args)) return(args)
    ## else
    args <- do.call(argsAnywhere, list(fname))
    if (is.null(args))
        character(0)
    else if (is.list(args))
        unlist(lapply(args, function(f) names(formals(f))))
    else
        names(formals(args))
}



specialFunctionArgs <- function(fun, text)
{
    ## certain special functions have special possible arguments.
    ## This is primarily applicable to library and require, for which
    ## rownames(installed.packages()).  This is disabled by default,
    ## because the first call to installed.packages() can be time
    ## consuming, e.g. on a network file system.  However, the results
    ## are cached, so subsequent calls are not that expensive.

    switch(EXPR = fun,

           library = ,
           require = {
               if (.CompletionEnv$settings[["ipck"]])
               {
                   grep(sprintf("^%s", makeRegexpSafe(text)),
                        rownames(installed.packages()), value = TRUE)
               }
               else character(0)
           },

           data = {
               if (.CompletionEnv$settings[["data"]])
               {
                   grep(sprintf("^%s", makeRegexpSafe(text)),
                        data()$results[, "Item"], value = TRUE)
               }
               else character(0)
           },

           ## otherwise,
           character(0))
}



functionArgs <-
    function(fun, text,
             S3methods = .CompletionEnv$settings[["S3"]],
             S4methods = FALSE,
             add.args = rc.getOption("funarg.suffix"))
{
    if (length(fun) < 1 || any(fun == "")) return(character(0))
    specialFunArgs <- specialFunctionArgs(fun, text)
    if (S3methods && exists(fun, mode = "function"))
        fun <-
            c(fun,
              tryCatch(methods(fun),
                       warning = function(w) {},
                       error = function(e) {}))
    if (S4methods)
        warning("Can't handle S4 methods yet")
    allArgs <- unique(unlist(lapply(fun, argNames)))
    ans <-
        grep(sprintf("^%s", makeRegexpSafe(text)),
             allArgs, value = TRUE)
    if (length(ans) > 0 && !is.null(add.args))
        ans <- sprintf("%s%s", ans, add.args)
    c(specialFunArgs, ans)
}



## Note: Inside the C code, we redefine
## rl_attempted_completion_function rather than
## rl_completion_entry_function, which means that if
## retrieveCompletions() returns a length-0 result, by default the
## fallback filename completion mechanism will be used.  This is not
## quite the best way to go, as in certain (most) situations filename
## completion will definitely be inappropriate even if no valid R
## completions are found.  We could return "" as the only completion,
## but that produces an irritating blank line on
## list-possible-completions (or whatever the correct name is).
## Instead (since we don't want to reinvent the wheel), we use the
## following scheme: If the character just preceding our token is " or
## ', we immediately go to file name completion.  If not, we do our
## stuff, and disable file name completion (using
## .Call("RCSuppressFileCompletion")) even if we don't find any
## matches.

## Note that under this scheme, filename completion will fail
## (possibly in unexpected ways) if the partial name contains 'unusual
## characters', namely ones that have been set (see C code) to cause a
## word break because doing so is meaningful in R syntax (e.g. "+",
## "-" ("/" is exempt (and treated specially below) because of its
## ubiquitousness in UNIX file names (where this package is most
## likely to be used))


## decide whether to fall back on filename completion.  Yes if the
## number of quotes between the cursor and the beginning of the line
## is an odd number.

fileCompletionPreferred <- function()
{
    ((st <- .CompletionEnv[["start"]]) > 0 && {
        
        linebuffer <- .CompletionEnv[["linebuffer"]]
        lbss <- head(unlist(strsplit(linebuffer, "")), .CompletionEnv[["end"]])
        ((sum(lbss == "'") %% 2 == 1) ||
         (sum(lbss == '"') %% 2 == 1))

    })
}


## .completeToken() is the primary interface, and does the actual
## completion when called from C code.


.completeToken <- function()
{
    text <- .CompletionEnv[["token"]]
    if (fileCompletionPreferred())
    {

        ## If we're in here, that means we think standard filename
        ## completion is more appropriate (by design, this is supposed
        ## to happen when we're inside quotes, which is not exactly
        ## equivalent, but there's not much else we can do anyway in
        ## that case).  We make no attempt to do filename completion
        ## because other people have written better code to do that,
        ## and so we set our completion list to be empty.  Third party
        ## software using this code can interrogate
        ## rc.status("fileName") to determine if this is the situation
        ## and act accordingly.  It's probably even OK to fill
        ## .CompletionEnv$comps with something suitable.

        .CompletionEnv[["comps"]] <- character(0)
        .setFileComp(TRUE)
    }
    else
    {
        .setFileComp(FALSE)
        setIsFirstArg(FALSE) # might be changed by inFunction() call
        ## make a guess at what function we are inside
        guessedFunction <-
            if (.CompletionEnv$settings[["args"]])
                inFunction(.CompletionEnv[["linebuffer"]],
                           .CompletionEnv[["start"]])
            else ""
        .CompletionEnv[["fguess"]] <- guessedFunction

        ## if this is not "", then we want to add possible arguments
        ## of that function(s) (methods etc).  Should be character(0)
        ## if nothing matches

        fargComps <- functionArgs(guessedFunction, text)

        if (getIsFirstArg() &&
            guessedFunction %in%
            c("library", "require", "data"))
        {
            ## don't try anything else
            .CompletionEnv[["comps"]] <- fargComps
            return()
        }

        ## Is there a "/" in there?  If so, work on the part after
        ## that and append to prefix before returning.  See note in
        ## rcompletion.c on why / is treated specially while + etc are
        ## not (hint: filename completion)

        lastSlash <- tail(gregexpr("/", text, fixed = TRUE)[[1]], 1)
        if (haveSlash <- (lastSlash > 0))
        {
            prefix <- substr(text, 1, lastSlash)
            text <- substr(text, lastSlash + 1, 1e6)
        }

        spl <- specialOpLocs(text)
        comps <-
            if (length(spl) > 0)
                specialCompletions(text, spl)
            else
            {
                ## should we append a left-paren for functions?
                ## Usually yes, but not when inside certain special
                ## functions which often take other functions as
                ## arguments

                appendFunctionSuffix <-
                    !any(guessedFunction %in%

                         c("help", "args", "formals", "example",
                           "do.call", "environment", "page", "apply",
                           "sapply", "lapply", "tapply", "mapply",
                           "methods", "fix", "edit"))

                normalCompletions(text, check.mode = appendFunctionSuffix)
            }
        if (haveSlash && length(comps) > 0)
        {
            comps <- paste(prefix, comps, sep = "")
        }
        comps <- c(comps, fargComps)
        .CompletionEnv[["comps"]] <- comps
    }
}


