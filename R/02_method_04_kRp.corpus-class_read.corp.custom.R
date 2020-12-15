# Copyright 2015-2020 Meik Michalke <meik.michalke@hhu.de>
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


#' Apply read.corp.custom() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:read.corp.custom]{read.corp.custom}} on all tagged text objects
#' inside the given \code{corpus} object.
#' 
#' Since the analysis is based on a document term matrix, a pre-existing matrix as a feature of the \code{corpus} object 
#' will be used if it matches the case sensitivity setting. Otherwise a new matrix will be generated (but not replace the
#' existing one). If no document term matrix is present yet, also one will be generated and can be kept as an additional feature
#' of the resulting object.
#' 
#' @param corpus An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param caseSens Logical. If \code{FALSE}, all tokens will be matched in their lower case form.
#' @param log.base A numeric value defining the base of the logarithm used for inverse document frequency (idf). See
#'    \code{\link[base:log]{log}} for details.
#' @param keep_dtm Logical. If \code{TRUE} and \code{corpus} does not yet provide a
#'    \code{\link[koRpus:docTermMatrix]{document term matrix}}, the one generated during calculation
#'    will be added to the resulting object.
#' @param ... Options to pass through to the \code{\link[koRpus:read.corp.custom]{read.corp.custom}} method
#'    for objects of the class union \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#' @return An object of the same class as \code{corpus}.
#' @export
#' @importFrom koRpus read.corp.custom kRp_text docTermMatrix
#' @include 01_class_01_kRp.corpus.R
#' @docType methods
#' @aliases read.corp.custom,kRp.corpus-method
#' @rdname read.corp.custom
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   myCorpus <- read.corp.custom(myCorpus)
#'   corpusCorpFreq(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("read.corp.custom", signature(corpus="kRp.corpus"), function(corpus, caseSens=TRUE, log.base=10, keep_dtm=FALSE, ...){
    dot_args <- list(...)
    if(!is.null(dot_args[["as.feature"]])){
      stop(simpleError("The argument \"as.feature\" can't be used!"))
    } else {}
    tagged_large <- kRp_text(
      lang=language(corpus),
      tokens=taggedText(corpus)
    )
    if(hasFeature(corpus, "doc_term_matrix")){
      reanalyze <- FALSE
      dtm_meta <- corpusMeta(corpus, meta="doc_term_matrix")
      if(!identical(dtm_meta[["case.sens"]], caseSens)){
        warning("Object features a document term matrix, but with different case sensitivity, need to re-analyze.")
        reanalyze <- TRUE
      } else {}
      if(isTRUE(dtm_meta[["tfidf"]])){
        warning("Object features a document term matrix, but with tf-idf values, need to re-analyze.")
        reanalyze <- TRUE
      } else {}
      if(isTRUE(reanalyze)){
        dtm <- docTermMatrix(corpus, case.sens=caseSens)
      } else {
        dtm <- corpusDocTermMatrix(corpus)
      }
    } else {
      if(isTRUE(keep_dtm)){
        corpus <- docTermMatrix(corpus, case.sens=caseSens)
        dtm <- corpusDocTermMatrix(corpus)
      } else {
        dtm <- corpusDocTermMatrix(docTermMatrix(corpus, case.sens=caseSens))
      }
    }
    corpusCorpFreq(corpus) <- read.corp.custom(tagged_large, caseSens=caseSens, log.base=log.base, dtm=dtm, ...)
    return(corpus)
  }
)
