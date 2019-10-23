# Copyright 2019 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package tm.plugin.koRpus.
#
# tm.plugin.koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# tm.plugin.koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tm.plugin.koRpus.  If not, see <http://www.gnu.org/licenses/>.

#' Generate a document-term matrix from a corpus object
#'
#' Returns a sparse document-term matrix calculated from a given object of class
#' \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}. You can also
#' calculate the term frequency inverted document frequency value (tf-idf) for each
#' term.
#' 
#' See the examples to learn how to limit the analysis to desired word classes.
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param terms A character string defining the \code{tokens} column to be used for calculating the matrix.
#' @param case.sens Logical, whether terms should be counted case sensitive.
#' @param tfidf Logical, if \code{TRUE} calculates term frequency--inverse document frequency (tf-idf)
#'   values instead of absolute frequency.
#' @return A sparse matrix of class \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.
#' @importFrom Matrix Matrix
#' @export
#' @docType methods
#' @rdname docTermMatrix
#' @examples
#' \dontrun{
#' myCorpus <- readCorpus(
#'   dir=file.path(path.package("tm.plugin.koRpus"), "tests", "testthat", "samples"),
#'   hierarchy=list(
#'     Topic=c(
#'       C3S="C3S",
#'       GEMA="GEMA"
#'     ),
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   )
#' )
#' 
#' # get the document-term frequencies in a sparse matrix
#' myDTMatrix <- docTermMatrix(myCorpus)
#' 
#' # combine with filterByClass() to, e.g.,  exclude all punctuation
#' myDTMatrix <- docTermMatrix(filterByClass(myCorpus))
#' 
#' # instead of absolute frequencies, get the tf-idf values
#' myDTMatrix <- docTermMatrix(
#'   filterByClass(myCorpus),
#'   tfidf=TRUE
#' )
#' }
setGeneric("docTermMatrix", function(obj, terms="token", case.sens=FALSE, tfidf=FALSE) standardGeneric("docTermMatrix"))

#' @rdname docTermMatrix
#' @docType methods
#' @export
#' @aliases
#'    docTermMatrix,-methods
#'    docTermMatrix,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("docTermMatrix",
  signature=signature(obj="kRp.corpus"),
  function(obj, terms="token", case.sens=FALSE, tfidf=FALSE){
    tagged <- tif_as_tokens_df(obj)
    if(!isTRUE(case.sens)){
      tagged[[terms]] <- tolower(tagged[[terms]])
    } else {}
    uniqueTerms <- unique(tagged[[terms]])
    doc_ids <- unique(as.character(tagged[["doc_id"]]))

    dt_mtx <- matrix(
      0,
      nrow=length(doc_ids),
      ncol=length(uniqueTerms),
      dimnames=list(doc_ids, uniqueTerms)
    )
    if(isTRUE(tfidf)){
      tf_mtx <- dt_mtx
    } else {}

    for (thisDoc in doc_ids){
      relevantTerms <- tagged[tagged[["doc_id"]] %in% thisDoc, terms]
      termsInDoc <- table(relevantTerms)
      if(isTRUE(tfidf)){
        tf_mtx[rownames(tf_mtx) %in% thisDoc, colnames(tf_mtx) %in% names(termsInDoc)] <- termsInDoc/length(relevantTerms)
      } else {}
      dt_mtx[rownames(dt_mtx) %in% thisDoc, colnames(dt_mtx) %in% names(termsInDoc)] <- termsInDoc
    }

    if(isTRUE(tfidf)){
      idf <- log(nrow(dt_mtx)/colSums(dt_mtx > 0))
      result <- Matrix(t(t(tf_mtx) * idf), sparse=TRUE)
    } else {
      result <- Matrix(dt_mtx, sparse=TRUE)
    }
    return(result)
  }
)
