\name{rcompletion}
\alias{rcompletion-package}
\title{Completion for \R using the GNU Readline library}

\description{
  This package provides pseudo-intelligent TAB completion for a readline
  enabled instance of \R when it is run from a terminal (or more
  specifically, an interface which uses readline to accept user input).
  It has no effect on the various GUI interfaces to \R, including ESS and
  the standard Windows interface.
}

\details{

  This package does not define any functions.  It is of interest solely
  for the wonderful things that start to happen when you press the TAB
  key inside \R after loading this package.  Strictly speaking, this
  package only provides a binding to the GNU Readline library, and uses
  the \code{rcompgen} package to generate potential completions.  That
  package exports a few functions (notably
  \code{\link[rcompgen]{rc.settings}}), providing a simple control and
  query mechanism, that are also available when the \code{rcompletion}
  package is attached.  The help page for those functions should be
  consulted for details (some of which this page refers to in passing).

  There are several types of completion, some of which can be disabled
  using \code{rc.settings}.  The most basic level, which can not be
  turned off once the package is loaded, provides completion on names
  visible on the search path, along with a few special keywords
  (e.g. \code{TRUE}).  This type of completion is not attempted if the
  partial \sQuote{word} (a.k.a. token) being completed is empty (since
  there would be too many completions).  The more advanced types of
  completion are briefly described below (and in more detail in the help
  page for \code{\link[rcompgen]{rcompgen}}).

  
  \describe{

    \item{\bold{Completion after extractors \code{$} and \code{@}}:}{
      When the \code{ops} setting is turned on, completion after
      \code{$} and \code{@} is attempted.
    }

    \item{\bold{Completion inside namespaces}:}{
      When the \code{ns} setting is turned on, completion inside
      namespaces is attempted when a token is preceded by the \code{::}
      or \code{:::} operators.  Additionally, the basic completion
      mechanism is extended to include attached namespaces, or more
      precisely, \code{foopkg::} becomes a valid completion of
      \code{foo} if the return value of \code{\link{search}()} includes
      the string \code{"package:foopkg"}.
    }

    \item{\bold{Completion of function arguments}:}{
      When the \code{args} setting is turned on, completion on function
      arguments is attempted whenever deemed appropriate.  The list of
      possible argument completions that is generated can be misleading.
    }
  }
}

\note{

  This file assumes throughout that the TAB key is bound to the default
  readline \code{complete} function, but this can be modified through an
  \code{inputrc} file.  The details are dicussed in the Bash reference
  manual, under \dQuote{Readline Initialization} (under \dQuote{Command
    Line Editing}) and the subsequent sections.  Conditional constructs
  can use the application name \code{"RCustomCompletion"} for settings
  affecting this package.

}

\seealso{
  \code{\link[rcompgen]{rc.settings}} for further details and utilities
  that allow customization.
}

\references{
  GNU Readline manual,
  \url{http://cnswww.cns.cwru.edu/php/chet/readline/readline.html}

  GNU Bash manual,
  \url{http://www.gnu.org/software/bash/manual/bashref.html}
}

\author{ Deepayan Sarkar, \email{deepayan.sarkar@r-project.org} }

\keyword{IO}
\keyword{utilities}
