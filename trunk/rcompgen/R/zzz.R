
.noGenerics <- TRUE

.CompletionEnv <- new.env()

## needed to save some overhead in .win32consoleCompletion
assign("linebuffer", "", env = .CompletionEnv)
assign("end", 1, env = .CompletionEnv)

assign("settings",
       list(ops = TRUE, ns = TRUE,
            args = TRUE, func = FALSE,
            ipck = FALSE, S3 = TRUE, data = TRUE,
            help = TRUE, argdb = TRUE),
       env = .CompletionEnv)


assign("options",
       list(package.suffix = "::",
            funarg.suffix = "=",
            function.suffix = "("),
       env = .CompletionEnv)

.FunArgEnv <- new.env(hash = TRUE, parent = emptyenv())

.initialize.argdb() ## see argdb.R


## .onLoad <- function(libname, pkgname)
## {
##     if (.Platform$OS.type == "windows")
##     {
##         message("Initializing completion utilities for Windows Rgui")
##         .setupStupidFileBasedCommunicationLayer()
##     }
## }

