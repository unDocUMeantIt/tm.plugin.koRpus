% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_method_03_kRp.corpus-class_lex.div.R
\docType{methods}
\name{lex.div,kRp.corpus-method}
\alias{lex.div,kRp.corpus-method}
\title{Apply lex.div() to all texts in kRp.corpus objects}
\usage{
\S4method{lex.div}{kRp.corpus}(
  txt,
  summary = TRUE,
  mc.cores = getOption("mc.cores", 1L),
  char = "",
  quiet = TRUE,
  ...
)
}
\arguments{
\item{txt}{An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.}

\item{summary}{Logical, determines if the \code{summary} slot should automatically be
updated by calling \code{\link[tm.plugin.koRpus:summary]{summary}} on the result.}

\item{mc.cores}{The number of cores to use for parallelization,
      see \code{\link[parallel:mclapply]{mclapply}}.}

\item{char}{Character vector to specify measures of which characteristics should be computed,
      see
\code{\link[koRpus:lex.div]{lex.div}} for details.}

\item{quiet}{Logical, if \code{FALSE} shows a status bar for some measures of each text,
      see
\code{\link[koRpus:lex.div]{lex.div}} for details.}

\item{...}{options to pass through to \code{\link[koRpus:lex.div]{lex.div}}.}
}
\value{
An object of the same class as \code{txt}.
}
\description{
This method calls \code{\link[koRpus:lex.div]{lex.div}} on all tagged text objects
inside the given \code{txt} object (using \code{mclapply}).
}
\examples{
# use readCorpus() to create an object of class kRp.corpus
# code is only run when the english language package can be loaded
if(require("koRpus.lang.en", quietly = TRUE)){
  myCorpus <- readCorpus(
    dir=file.path(path.package("tm.plugin.koRpus"), "examples", "corpus"),
    hierarchy=list(
      Topic=c(
        Winner="Reality Winner",
        Edwards="Natalie Edwards"
      ),
      Source=c(
        Wikipedia_prev="Wikipedia (old)",
        Wikipedia_new="Wikipedia (new)"
      )
    ),
    # use tokenize() so examples run without a TreeTagger installation
    tagger="tokenize",
    lang="en"
  )
  myCorpus <- lex.div(myCorpus)
  corpusSummary(myCorpus)
} else {}
}
