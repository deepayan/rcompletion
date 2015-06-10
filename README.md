# rcompletion

This project provides an implementation of context-specific command line completion for R on the bash shell using its [`bash_completion`](https://bash-completion.alioth.debian.org/) mechanism. It provides a single file that should be executed after the system bash completion has been loaded, usually by placing it inside `/usr/share/bash-completion/completions/` or `/etc/bash_completion.d/`, or explicitly loading it using 

```
$ . /path/to/bash_completion/R
```

inside `~/.bashrc`. 

## Examples of context-specific completions

```
$ R CMD b<TAB>
```
completes to `build`.
```
$ R CMD build --<TAB><TAB>
```
offers the list of options to `R CMD build`, which at the time of writing is
```
--compact-vignettes   --help                --md5                 --no-manual           --resave-data         
--force               --keep-empty-dirs     --no-build-vignettes  --no-resave-data      --version
```

## Note

In a previous life on Google Code, this project also implemented tools for code completion in the R REPL interface. These tools have since been merged into R itself.
