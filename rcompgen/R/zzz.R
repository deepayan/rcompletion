
.noGenerics <- TRUE

.CompletionEnv <- new.env()

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



