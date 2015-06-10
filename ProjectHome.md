In certain situations, R uses the GNU Readline library to provide advanced command line editing features. It is possible to extend these features, using facilities in Readline, to additionally provide smart command completion as well (rather than the simple file name completion enabled by default).  This project provides two small R packages, one implementing the infrastructure for generating completions, and another that binds the completions to readline.  This separation will hopefully allow the completion infrastructure to be used in other contexts as well.

**Update**:

The rcompletion (and rcompgen from R 2.7.0 onwards) are obsolete as packages.
The facilities provided by them should be available in recent
versions of R without the user needing to do anything.

The technical details are this:

  * part of rcompletion was incorporated into R-2.5.0 and the remaining part was put in package rcompgen (which was made a recommended package, so it would typically get installed along with R).

  * In R-2.7.0, even the rcompgen package will become obsolete, and it's code incorporated into the utils package.
