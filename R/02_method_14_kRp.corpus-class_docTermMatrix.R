# Copyright 2019-2020 Meik Michalke <meik.michalke@hhu.de>
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
#' Calculates a sparse document-term matrix calculated from a given object of class
#' \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}} and adds it to the object's feature list.
#' You can also calculate the term frequency inverted document frequency value (tf-idf) for each
#' term.
#' 
#' The settings of \code{terms}, \code{case.sens}, and \code{tfidf} will be stored in the object's \code{meta} slot,
#' so you can use \code{corpusMeta(..., "doc_term_matrix")} to fetch it.
#' 
#' See the examples to learn how to limit the analysis to desired word classes.
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param terms A character string defining the \code{tokens} column to be used for calculating the matrix.
#' @param case.sens Logical, whether terms should be counted case sensitive.
#' @param tfidf Logical, if \code{TRUE} calculates term frequency--inverse document frequency (tf-idf)
#'    values instead of absolute frequency.
#' @param as.feature Logical, whether the output should be just the sparse matrix or the input object with
#'    that matrix added as a feature. Use \code{\link[tm.plugin.koRpus:corpusDocTermMatrix]{corpusDocTermMatrix}}
#'    to get the matrix from such an aggregated object.
#' @return Either an object of the input class or a sparse matrix of class
#'    \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}}.
#' @importFrom koRpus docTermMatrix tif_as_tokens_df
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @aliases
#'    docTermMatrix,-methods
#'    docTermMatrix,kRp.corpus-method
#' @docType methods
#' @rdname docTermMatrix
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_4_texts.R
#' @examples
#'
#'   # get the document-term frequencies in a sparse matrix
#'   myDTMatrix <- docTermMatrix(myCorpus, as.feature=FALSE)
#' 
#'   # combine with filterByClass() to, e.g.,  exclude all punctuation
#'   myDTMatrix <- docTermMatrix(filterByClass(myCorpus), as.feature=FALSE)
#' 
#'   # instead of absolute frequencies, get the tf-idf values
#'   myDTMatrix <- docTermMatrix(
#'     filterByClass(myCorpus),
#'     tfidf=TRUE,
#'     as.feature=FALSE
#'   )
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("docTermMatrix",
  signature=signature(obj="kRp.corpus"),
  function(
    obj,
    terms="token",
    case.sens=FALSE,
    tfidf=FALSE,
    as.feature=TRUE
  ){
    result <- docTermMatrix(
      obj=tif_as_tokens_df(obj),
      terms=terms,
      case.sens=case.sens,
      tfidf=tfidf
    )

    if(isTRUE(as.feature)){
      corpusDocTermMatrix(
        obj,
        terms=terms,
        case.sens=case.sens,
        tfidf=tfidf
      ) <- result
      return(obj)
    } else {
      return(result)
    }
  }
)
