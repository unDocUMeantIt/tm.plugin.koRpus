% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_method_10_kRp.corpus-class_jumbleWords.R
\docType{methods}
\name{jumbleWords,kRp.corpus-method}
\alias{jumbleWords,kRp.corpus-method}
\title{Apply jumbleWords() to all texts in kRp.corpus objects}
\usage{
\S4method{jumbleWords}{kRp.corpus}(words, mc.cores = getOption("mc.cores", 1L), ...)
}
\arguments{
\item{words}{An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.}

\item{mc.cores}{The number of cores to use for parallelization,
      see \code{\link[parallel:mclapply]{mclapply}}.}

\item{...}{options to pass through to \code{\link[koRpus:jumbleWords]{jumbleWords}}.}
}
\value{
An object of the same class as \code{words}.
}
\description{
This method calls \code{\link[koRpus:jumbleWords]{jumbleWords}} on all tagged text objects
inside the given \code{words} object (using \code{mclapply}).
}
\examples{
# use readCorpus() to create an object of class kRp.corpus
# code is only run when the english language package can be loaded
if(require("koRpus.lang.en", quietly = TRUE)){
  myCorpus <- readCorpus(
    dir=file.path(
      path.package("tm.plugin.koRpus"), "examples", "corpus", "Edwards"
    ),
    hierarchy=list(
      Source=c(
        Wikipedia_prev="Wikipedia (old)",
        Wikipedia_new="Wikipedia (new)"
      )
    ),
    # use tokenize() so examples run without a TreeTagger installation
    tagger="tokenize",
    lang="en"
  )

  head(taggedText(myCorpus), n=10)
  myCorpus <- jumbleWords(myCorpus)
  head(taggedText(myCorpus), n=10)
} else {}
}
