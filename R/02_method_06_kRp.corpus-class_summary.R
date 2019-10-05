# Copyright 2015-2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Apply summary() to all texts in kRp.flatHier objects
#' 
#' This method performs a \code{summary} call on all text objects inside the given
#' \code{object} object. Contrary to what other summary methods do, this method
#' always returns the full object with an updated \code{summary} slot.
#' 
#' The \code{summary} slot contains a data.frame with aggregated information of
#' all texts that the respective object contains.
#' 
#' \code{corpusSummary} is a simple method to get or set the \code{summary} slot
#' in kRp.flatHier objects directly.
#' 
#' @param object An object of class \code{\link[tm.plugin.koRpus:kRp.flatHier-class]{kRp.flatHier}}.
# @param available.rdb Character vector with the column names of all readability measures
#    that are supposed to be available for all texts. Missings are automatically filled
#    with the value of \code{missing}.
# @param available.TTR Like \code{available.rdb}, only for lexical diversity measures.
#' @param missing Character string to use for missing values.
#' @param ... Used for internal processes.
#' @return An object of the same class as \code{object}.
#' @importFrom NLP meta
#' @export
#' @docType methods
#' @aliases summary,kRp.flatHier-method
#' @rdname summary
#' @examples
#' \dontrun{
#' myCorpus <- readCorpus(
#'   dir=file.path(
#'     path.package("tm.plugin.koRpus"), "tests", "testthat", "samples", "C3S"
#'   ),
#'   hierarchy=list(
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   )
#' )
#' myCorpus <- readability(myCorpus, summary=FALSE)
#' corpusSummary(myCorpus)
#' # add summaries
#' myCorpus <- summary(myCorpus)
#' corpusSummary(myCorpus)
#' }
#' @include 01_class_01_kRp.flatHier.R
setMethod("summary", signature(object="kRp.flatHier"), function(
  object, missing=NA, ...
){
    available.rdb <- nullToList(unlist(corpusMeta(object, "readability", fail=FALSE)[["index"]]), entry="index")
    available.TTR <- nullToList(unlist(corpusMeta(object, "TTR", fail=FALSE)[["index"]]), entry="index")

    # initialize the data.frame
    summary.info <- meta(corpusTm(object))[, c("doc_id", names(slot(object, "hierarchy")))]
    summary.info[["stopwords"]] <- corpusMeta(object, "stopwords")

    summary.rdb <- summary.lexdiv <- NULL

    if(!is.null(available.rdb)){
      if(length(available.rdb[["index"]]) > 0){
        summary.rdb <- t(as.data.frame(sapply(names(corpusReadability(object)), function(thisText){
            thisSummary <- summary(corpusReadability(object)[[thisText]], flat=TRUE)
            return(fixMissingIndices(have=thisSummary, want=available.rdb[["index"]], missing=missing))
        }, simplify=FALSE)))
        summary.info <- cbind(
          summary.info,
          getRdbDesc(object),
          summary.rdb
        )
      } else {}
    } else {}
    if(!is.null(available.TTR)){
      if(length(available.TTR[["index"]]) > 0){
        summary.lexdiv <- t(as.data.frame(sapply(names(corpusTTR(object)), function(thisText){
            thisSummary <- summary(corpusTTR(object)[[thisText]], flat=TRUE)
            return(fixMissingIndices(have=thisSummary, want=available.TTR[["index"]], missing=missing))
        }, simplify=FALSE)))
        # suppress a second TTR
        if("TTR" %in% colnames(summary.info) & "TTR" %in% colnames(summary.lexdiv)){
          summary.lexdiv <- subset(summary.lexdiv, select=-TTR)
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
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.flatHier-class]{kRp.flatHier}}.
#' @docType methods
#' @export
setGeneric("corpusSummary", function(obj) standardGeneric("corpusSummary"))
#' @rdname summary
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.flatHier-method
#' @include 01_class_01_kRp.flatHier.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.flatHier"),
  function (obj){
    result <- slot(obj, name="summary")
    return(result)
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
#'    corpusSummary<-,kRp.flatHier-method
#' @include 01_class_01_kRp.flatHier.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.flatHier"),
  function (obj, value){
    slot(obj, name="summary") <- value
    return(obj)
  }
)
