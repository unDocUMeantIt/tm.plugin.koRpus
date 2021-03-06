% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_method_15_kRp.corpus-class_split_by_doc_id.R
\docType{methods}
\name{split_by_doc_id,kRp.corpus-method}
\alias{split_by_doc_id,kRp.corpus-method}
\alias{split_by_doc_id,-methods}
\title{Turn a kRp.corpus object into a list of kRp.text objects}
\usage{
\S4method{split_by_doc_id}{kRp.corpus}(obj, keepFeatures = TRUE)
}
\arguments{
\item{obj}{An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.}

\item{keepFeatures}{Either logical, whether to keep all features or drop them,
      or a character vector
of names of features to keep if present.}
}
\value{
A named list of objects of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
   Elements are named by their \code{doc_id}.
}
\description{
For some analysis steps it might be important to have individual tagged texts
instead of one large corpus object. This method produces just that.
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

  myCorpusList <- split_by_doc_id(myCorpus)
} else {}
}
