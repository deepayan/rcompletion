
.noGenerics <- TRUE

.CompletionEnv <- new.env()

assign("settings",
       list(ops = TRUE, ns = TRUE, args = TRUE),
       env = .CompletionEnv)


assign("options",
       list(package.suffix = "::",
            funarg.suffix = " = ",
            function.suffix = "("),
       env = .CompletionEnv)

