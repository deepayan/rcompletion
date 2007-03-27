
## support functions that attempt to provide tools useful specifically
## for the Windows Rgui.  The first approach, imlemented by the first
## two functions below, is an extremely clumsy file based one.  The
## third function is probably the direction we want to go in.



.setupStupidFileBasedCommunicationLayer <- function()
{
    tmpdir <- tempdir()
    compdir <- ".compgen"
    longcompdir <- file.path(tmpdir, compdir)
    Sys.setenv(RCOMPGEN_DIR = longcompdir)
    Sys.setenv(RCOMPGEN_DONE = "no")

    dir.create(longcompdir)
    file.create(file.path(longcompdir, "incompleteLine"))
    file.create(file.path(longcompdir, "cursorPosition"))
    file.create(file.path(longcompdir, "additionalText"))
}


.performStupidFileBasedCommunication <- function()
{
    longcompdir <- Sys.getenv("RCOMPGEN_DIR")
    
    linebuffer <- tail(readLines(file.path(longcompdir, "incompleteLine"), warn = FALSE), 1)
    ## linebuffer <- readLines(file.path(longcompdir, "incompleteLine"), warn = FALSE)[1]
    cursorPosition <- as.integer(scan(file.path(longcompdir, "cursorPosition"), nmax = 1, quiet = TRUE))
    
    .assignLinebuffer(linebuffer)
    .assignEnd(cursorPosition)
    .guessTokenFromLine()
    .completeToken()
    comps <- .retrieveCompletions()

    ## FIXME: no idea how much of this is MBCS-safe

    token <- .CompletionEnv[["token"]]
    if (length(comps) == 0)
    {
        ## no completions
        cat("", file = file.path(longcompdir, "additionalText"), fill = FALSE)
    }
    else if (length(comps) == 1)
    {

        ## FIXME (maybe): in certain cases the completion may be
        ## shorter than the token (e.g. when trying to complete on an
        ## impossible name inside a list).  It's debatable what the
        ## behaviour should be in this case, but readline and Emacs
        ## actually delete part of the token (at least currently).  To
        ## achieve this in Rgui one would need to do somewhat more
        ## work than I'm ready to do right now (especially since it's
        ## not clear that this is the right thing to do to begin
        ## with).  So, in this case, I'll just pretend that no
        ## completion was found.

        addition <- substr(comps, nchar(token) + 1, 100000)
        cat(addition, file = file.path(longcompdir, "additionalText"), fill = FALSE)

##         prefix <- substr(linebuffer, 1, .CompletionEnv[["start"]])
##         suffix <- substr(linebuffer, .CompletionEnv[["end"]] + 1, 100000)
##         compline <- paste(prefix, comps, suffix, collapse = "")
##         cursorpos <- nchar(prefix) + nchar(comps)
##         cat(compline, file = file.path(longcompdir, "completeLine"))
##         cat("", file = file.path(longcompdir, "possibleCompletions"))
##         cat(cursorpos, file = file.path(longcompdir, "cursorPosition"))
        
    }
    else if (length(comps) > 1)
    {

        ## more than one completion.  The right thing to is to extend
        ## the line by the unique part if any, and list the multiple
        ## possibilities otherwise.

        additions <- substr(comps, nchar(token) + 1, 100000)
        if (length(table(substr(additions, 1, 1))) > 1)
        {
            ## no unique substring
            cat(comps, file = "", fill = TRUE)
            cat("", file = file.path(longcompdir, "additionalText"), fill = FALSE)
        }
        else
        {
            ## need to figure out maximal unique substr
            keepUpto <- 1
            while (length(table(substr(additions, 1, keepUpto))) == 1) keepUpto <- keepUpto + 1
            addition <- substr(additions[1], 1, keepUpto - 1)
            cat(addition, file = file.path(longcompdir, "additionalText"), fill = FALSE)
        }
    }
    Sys.setenv(RCOMPGEN_DONE = "yes")
}






.win32consoleCompletion <- function(linebuffer, cursorPosition)
{
    isRepeat <- ## is TAB being pressed for the first time with this combination?
        (linebuffer == .CompletionEnv[["linebuffer"]] &&
         cursorPosition == .CompletionEnv[["end"]])
    
    .assignLinebuffer(linebuffer)
    .assignEnd(cursorPosition)
    .guessTokenFromLine()
    .completeToken()
    comps <- .retrieveCompletions()

    ## FIXME: no idea how much of this is MBCS-safe

    token <- .CompletionEnv[["token"]]
    if (length(comps) == 0)
    {
        ## no completions
        addition <- ""
        possible <- character(0)
    }
    else if (length(comps) == 1)
    {
        ## FIXME (maybe): in certain cases the completion may be
        ## shorter than the token (e.g. when trying to complete on an
        ## impossible name inside a list).  It's debatable what the
        ## behaviour should be in this case, but readline and Emacs
        ## actually delete part of the token (at least currently).  To
        ## achieve this in Rgui one would need to do somewhat more
        ## work than I'm ready to do right now (especially since it's
        ## not clear that this is the right thing to do to begin
        ## with).  So, in this case, I'll just pretend that no
        ## completion was found.

        addition <- substr(comps, nchar(token) + 1, 100000)
        possible <- character(0)
    }
    else if (length(comps) > 1)
    {
        ## more than one completion.  The right thing to is to extend
        ## the line by the unique part if any, and list the multiple
        ## possibilities otherwise.

        additions <- substr(comps, nchar(token) + 1, 100000)
        if (length(table(substr(additions, 1, 1))) > 1)
        {
            ## no unique substring
            addition <- ""
            possible <-
                if (isRepeat) capture.output(cat(format(comps, justify = "left"), fill = TRUE))
                else character(0)
        }
        else
        {
            ## need to figure out maximal unique substr
            keepUpto <- 1
            while (length(table(substr(additions, 1, keepUpto))) == 1) keepUpto <- keepUpto + 1
            addition <- substr(additions[1], 1, keepUpto - 1)
            possible <- character(0)
        }
    }
    list(addition = addition, possible = possible)
}


