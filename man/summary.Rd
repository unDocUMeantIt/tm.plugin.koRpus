% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_method_06_kRp.corpus-class_summary.R
\docType{methods}
\name{summary,kRp.corpus-method}
\alias{summary,kRp.corpus-method}
\alias{corpusSummary}
\alias{corpusSummary,kRp.corpus-method}
\alias{corpusSummary,-methods}
\alias{corpusSummary<-}
\alias{corpusSummary<-,kRp.corpus-method}
\alias{corpusSummary<-,-methods}
\title{Apply summary() to all texts in kRp.corpus objects}
\usage{
\S4method{summary}{kRp.corpus}(object, missing = NA, ...)

corpusSummary(obj)

\S4method{corpusSummary}{kRp.corpus}(obj)

corpusSummary(obj) <- value

\S4method{corpusSummary}{kRp.corpus}(obj) <- value
}
\arguments{
\item{object}{An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.}

\item{missing}{Character string to use for missing values.}

\item{...}{Used for internal processes.}

\item{obj}{An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.}

\item{value}{The new value to replace the current with.}
}
\value{
An object of the same class as \code{object}.
}
\description{
This method performs a \code{summary} call on all text objects inside the given
\code{object} object. Contrary to what other summary methods do, this method
always returns the full object with an updated \code{summary} slot.
}
\details{
The \code{summary} slot contains a data.frame with aggregated information of
all texts that the respective object contains.

\code{corpusSummary} is a simple method to get or set the \code{summary} slot
in kRp.corpus objects directly.
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

  # calculate readability, but prevent a summary table from being added
  myCorpus <- readability(myCorpus, summary=FALSE)
  corpusSummary(myCorpus)

  # add summaries
  myCorpus <- summary(myCorpus)
  corpusSummary(myCorpus)
} else {}
}
