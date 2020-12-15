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

#' Apply summary() to all texts in kRp.corpus objects
#' 
#' This method performs a \code{summary} call on all text objects inside the given
#' \code{object} object. Contrary to what other summary methods do, this method
#' always returns the full object with an updated \code{summary} slot.
#' 
#' The \code{summary} slot contains a data.frame with aggregated information of
#' all texts that the respective object contains.
#' 
#' \code{corpusSummary} is a simple method to get or set the \code{summary} slot
#' in kRp.corpus objects directly.
#' 
#' @param object An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
# @param available_rdb Character vector with the column names of all readability measures
#    that are supposed to be available for all texts. Missings are automatically filled
#    with the value of \code{missing}.
# @param available_lex_div Like \code{available_rdb}, only for lexical diversity measures.
#' @param missing Character string to use for missing values.
#' @param ... Used for internal processes.
#' @return An object of the same class as \code{object}.
#' @importFrom NLP meta
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases summary,kRp.corpus-method
#' @rdname summary
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   # calculate readability, but prevent a summary table from being added
#'   myCorpus <- readability(myCorpus, summary=FALSE)
#'   corpusSummary(myCorpus)
#'
#'   # add summaries
#'   myCorpus <- summary(myCorpus)
#'   corpusSummary(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("summary", signature(object="kRp.corpus"), function(
  object, missing=NA, ...
){
    available_rdb <- nullToList(unlist(corpusMeta(object, "readability", fail=FALSE)[["index"]]), entry="index")
    available_lex_div <- nullToList(unlist(corpusMeta(object, "lex_div", fail=FALSE)[["index"]]), entry="index")

    # initialize the data.frame
    summary.info <- meta(corpusTm(object))[, c("doc_id", names(corpusHierarchy(object))), drop=FALSE]
    if(hasFeature(object, "stopwords")){
      summary.info[["stopwords"]] <- corpusStopwords(object)[["sum"]]
    } else {}

    summary.rdb <- summary.lexdiv <- NULL

    if(!is.null(available_rdb)){
      if(length(available_rdb[["index"]]) > 0){
        summary.rdb <- t(as.data.frame(sapply(names(corpusReadability(object)), function(thisText){
            thisSummary <- summary(corpusReadability(object)[[thisText]], flat=TRUE)
            return(fixMissingIndices(have=thisSummary, want=available_rdb[["index"]], missing=missing))
        }, simplify=FALSE)))
        summary.info <- cbind(
          summary.info,
          getRdbDesc(object),
          summary.rdb
        )
      } else {}
    } else {}
    if(!is.null(available_lex_div)){
      if(length(available_lex_div[["index"]]) > 0){
        summary.lexdiv <- t(as.data.frame(sapply(names(corpusLexDiv(object)), function(thisText){
            thisSummary <- summary(corpusLexDiv(object)[[thisText]], flat=TRUE)
            return(fixMissingIndices(have=thisSummary, want=available_lex_div[["index"]], missing=missing))
        }, simplify=FALSE)))
        # suppress a second TTR
        if("TTR" %in% colnames(summary.info) & "TTR" %in% colnames(summary.lexdiv)){
          summary.lexdiv <- summary.lexdiv[, !(names(summary.lexdiv) %in% "TTR")]
        } else {}
        summary.info <- cbind(
          summary.info,
          summary.lexdiv
        )
      } else {}
    }

    if(!is.null(summary.info)){
      rownames(summary.info) <- as.character(summary.info[["doc_id"]])
    } else {
      summary.info <- data.frame()
    }
    
    corpusSummary(object) <- summary.info

    return(object)
  }
)


#' @rdname summary
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @docType methods
#' @export
setGeneric("corpusSummary", function(obj) standardGeneric("corpusSummary"))
#' @rdname summary
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.corpus"),
  function (obj){
    return(feature(obj, "summary"))
  }
)

#' @rdname summary
#' @export
#' @docType methods
#' @param value The new value to replace the current with.
setGeneric("corpusSummary<-", function(obj, value) standardGeneric("corpusSummary<-"))
#' @rdname summary
#' @export
#' @docType methods
#' @aliases
#'    corpusSummary<-,-methods
#'    corpusSummary<-,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.corpus"),
  function (obj, value){
    feature(obj, "summary") <- value
    return(obj)
  }
)
