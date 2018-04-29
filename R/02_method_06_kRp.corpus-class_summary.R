# Copyright 2015-2018 Meik Michalke <meik.michalke@hhu.de>
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
#' This method performs a \code{summary} call on all text objects
#' inside the given \code{object} object (using \code{lapply}). Contrary to what
#' other summary methods do, these methods always return the full object with
#' an updated \code{summary} slot.
#' 
#' The methods for nested object classes also recursively invoke the summary
#' methods for lower corpus object classes; i.e., if you call \code{summary}
#' on an object of class \code{kRp.topicCorpus}, the nested objects of class
#' \code{kRp.sourcesCorpus} will also be summarised, and that in turn causes
#' summaries on all nested corpora of class \code{kRp.corpus}.
#' 
#' The \code{summary} slot contains a data.frame with aggregated information of
#' all texts that the respective object level contains.
#' 
#' @param object An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}},
#'    \code{\link[tm.plugin.koRpus:kRp.sourcesCorpus-class]{kRp.sourcesCorpus}} or
#'    \code{\link[tm.plugin.koRpus:kRp.topicCorpus-class]{kRp.topicCorpus}}.
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
#' @aliases summary summary,kRp.corpus-method
#' @rdname summary-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' summary(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
setMethod("summary", signature(object="kRp.corpus"), function(
  object, missing=NA, ...
){
    # initialize the data.frame
    summary.info <- data.frame(
      doc_id=meta(corpusTm(object))[["textID"]],
      topic=corpusMeta(object, "topic"),
      source=corpusMeta(object, "source"),
      stopwords=corpusMeta(object, "stopwords")
    )

    summary.rdb <- summary.lexdiv <- NULL

    available <- availableFromOptions(allOptions=list(...), object=object)
    available.rdb <- available[["available.rdb"]]
    available.TTR <- available[["available.TTR"]]

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
    } else {}

    if(!is.null(summary.info)){
      rownames(summary.info) <- as.character(summary.info[["doc_id"]])
    } else {
      summary.info <- data.frame()
    }
    
    corpusSummary(object) <- summary.info
    return(object)
  }
)

#' @aliases summary,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname summary-methods
#' @export
setMethod("summary", signature(object="kRp.sourcesCorpus"), function(object, ...){
    all.corpora <- corpusSources(object)

    # to not run into issues because of missing measures,
    # globally set the values
    available <- whatIsAvailable(all.corpora=all.corpora, level="sources")
    available <- availableFromOptions(allOptions=list(...), object=available)
    
    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- summary(all.corpora[[thisCorpus]],
        available.rdb=available[["available.rdb"]],
        available.TTR=available[["available.TTR"]])
    }
    corpusSources(object) <- all.corpora

    allSummary <- corpusSummary(all.corpora[[1]])
    for (thisSummary in all.corpora[-1]){
      allSummary <- rbind(allSummary, corpusSummary(thisSummary))
    }
    corpusSummary(object) <- allSummary

    return(object)
  }
)

#' @aliases summary,kRp.topicCorpus-method
#' @docType methods
#' @rdname summary-methods
#' @export
setMethod("summary", signature(object="kRp.topicCorpus"), function(object){
    all.topics <- corpusTopics(object)

    # to not run into issues because of missing measures,
    # globally set the values
    available <- whatIsAvailable(all.corpora=all.topics, level="topics")

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- summary(all.topics[[thisTopic]],
        available.rdb=available[["available.rdb"]],
        available.TTR=available[["available.TTR"]])
    }
    corpusTopics(object) <- all.topics

    allSummary <- corpusSummary(all.topics[[1]])
    for (thisSummary in all.topics[-1]){
      allSummary <- rbind(allSummary, corpusSummary(thisSummary))
    }
    corpusSummary(object) <- allSummary

    return(object)
  }
)

#' @aliases summary,kRp.hierarchy-method
#' @docType methods
#' @rdname summary-methods
#' @export
setMethod("summary", signature(object="kRp.hierarchy"), function(
  object, missing=NA, ...
){
    currentLevel <- corpusLevel(object)
    if(currentLevel > 0){
      all.children <- corpusChildren(object)

      # to not run into issues because of missing measures,
      # globally set the values
      # first check if there's availability info in the call options already
      if(any(c("available.rdb", "available.TTR") %in% names(list(...)))){
        available <- availableFromOptions(allOptions=list(...), object=object)
      } else {
        available <- whatIsAvailable(all.corpora=object, hierarchy=TRUE)
      }

      for (thisChild in names(all.children)){
        all.children[[thisChild]] <- summary(all.children[[thisChild]],
          available.rdb=available[["available.rdb"]],
          available.TTR=available[["available.TTR"]]
        )
      }
      corpusChildren(object) <- all.children

      allSummary <- corpusSummary(all.children[[1]])
      for (thisSummary in all.children[-1]){
        allSummary <- rbind(allSummary, corpusSummary(thisSummary))
      }
      corpusSummary(object) <- allSummary
    } else {
      # initialize the data.frame
      hierarchy_branch <- corpusMeta(object)[["hierarchy_branch"]]
      hierarchy_branch_id <- hierarchy_branch["id",]
      hierarchy_branch_names <- colnames(hierarchy_branch)
      text_IDs <- meta(corpusTm(object))[["textID"]]
      summary.info <- as.data.frame(
        matrix(
          hierarchy_branch_id,
          nrow=length(text_IDs),
          ncol=length(hierarchy_branch_id),
          byrow=TRUE,
          dimnames=list(
            c(),
            hierarchy_branch_names
          )
        )
      )
      summary.info[["doc_id"]] <- text_IDs
      summary.info[["stopwords"]] <- corpusMeta(object, "stopwords")

      summary.rdb <- summary.lexdiv <- NULL

      available <- availableFromOptions(allOptions=list(...), object=object)
      available.rdb <- available[["available.rdb"]]
      available.TTR <- available[["available.TTR"]]

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
      } else {}

      if(!is.null(summary.info)){
        rownames(summary.info) <- as.character(summary.info[["doc_id"]])
      } else {
        summary.info <- data.frame()
      }
      
      corpusSummary(object) <- summary.info
    }
    return(object)
  }
)


#' @rdname summary-methods
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}},
#'    \code{\link[tm.plugin.koRpus:kRp.sourcesCorpus-class]{kRp.sourcesCorpus}} or
#'    \code{\link[tm.plugin.koRpus:kRp.topicCorpus-class]{kRp.topicCorpus}}.
#' @docType methods
#' @export
setGeneric("corpusSummary", function(obj) standardGeneric("corpusSummary"))
#' @rdname summary-methods
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.corpus"),
  function (obj){
    result <- slot(obj, name="summary")
    return(result)
  }
)

#' @rdname summary-methods
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.sourcesCorpus-method
#' @include 01_class_02_kRp.sourcesCorpus.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.sourcesCorpus"),
  function (obj){
    result <- slot(obj, name="summary")
    return(result)
  }
)

#' @rdname summary-methods
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.topicCorpus-method
#' @include 01_class_03_kRp.topicCorpus.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.topicCorpus"),
  function (obj){
    result <- slot(obj, name="summary")
    return(result)
  }
)

#' @rdname summary-methods
#' @docType methods
#' @export
#' @aliases
#'    corpusSummary,-methods
#'    corpusSummary,kRp.hierarchy-method
#' @include 01_class_04_kRp.hierarchy.R
setMethod("corpusSummary",
  signature=signature(obj="kRp.hierarchy"),
  function (obj){
    result <- slot(obj, name="summary")
    return(result)
  }
)

#' @rdname summary-methods
#' @export
#' @docType methods
#' @param value The new value to replace the current with.
setGeneric("corpusSummary<-", function(obj, value) standardGeneric("corpusSummary<-"))
#' @rdname summary-methods
#' @export
#' @docType methods
#' @aliases
#'    corpusSummary<-,-methods
#'    corpusSummary<-,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.corpus"),
  function (obj, value){
    slot(obj, name="summary") <- value
    return(obj)
  }
)

#' @rdname summary-methods
#' @export
#' @docType methods
#' @aliases
#'    corpusSummary<-,-methods
#'    corpusSummary<-,kRp.sourcesCorpus-method
#' @include 01_class_02_kRp.sourcesCorpus.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.sourcesCorpus"),
  function (obj, value){
    slot(obj, name="summary") <- value
    return(obj)
  }
)

#' @rdname summary-methods
#' @export
#' @docType methods
#' @aliases
#'    corpusSummary<-,-methods
#'    corpusSummary<-,kRp.topicCorpus-method
#' @include 01_class_03_kRp.topicCorpus.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.topicCorpus"),
  function (obj, value){
    slot(obj, name="summary") <- value
    return(obj)
  }
)

#' @rdname summary-methods
#' @export
#' @docType methods
#' @aliases
#'    corpusSummary<-,-methods
#'    corpusSummary<-,hierarchy-method
#' @include 01_class_03_kRp.hierarchy.R
setMethod("corpusSummary<-",
  signature=signature(obj="kRp.hierarchy"),
  function (obj, value){
    slot(obj, name="summary") <- value
    return(obj)
  }
)
