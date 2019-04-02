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

#' Generate a document-term matrix from a corpus opbject
#'
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param terms A character string defining the \code{TT.res} column to be used for calculating the matrix.
#' @param case.sens Logical, whether terms should be counted case sensitive.
#' @return A sparse matrix of class \code{\link[Matrix:dgCMatrix]{dgCMatrix}}.
#' @importFrom Matrix Matrix
#' @export
#' @docType methods
#' @rdname docTermMatrix
# @examples
# \dontrun{
# }
setGeneric("docTermMatrix", function(obj, terms="token", case.sens=FALSE, ...) standardGeneric("docTermMatrix"))

#' @rdname docTermMatrix
#' @docType methods
#' @export
#' @aliases
#'    docTermMatrix,-methods
#'    docTermMatrix,kRp.hierarchy-method
#' @include 01_class_01_kRp.hierarchy.R
setMethod("docTermMatrix",
  signature=signature(obj="kRp.hierarchy"),
  function(obj, terms="token", case.sens=FALSE){
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
    for (thisDoc in doc_ids){
      termsInDoc <- table(tagged[tagged[["doc_id"]] %in% thisDoc, terms])
      dt_mtx[rownames(dt_mtx) %in% thisDoc, colnames(dt_mtx) %in% names(termsInDoc)] <- termsInDoc
    }
    result <- Matrix(dt_mtx, sparse=TRUE)
    return(result)
  }
)
