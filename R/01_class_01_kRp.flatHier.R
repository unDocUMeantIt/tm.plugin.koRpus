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


## function init_flatHier_TT.res()
# initializes the TT.res data frame including columns with hierarchy information
init_flatHier_TT.res <- function(hierarchy=list()){
  kRp_df <- koRpus::taggedText(koRpus::kRp_tagged())
  if(length(hierarchy) > 0){
    hier_names <- names(hierarchy)
    invalidNames <- hier_names %in% names(kRp_df)
    if(any(invalidNames)){
      stop(simpleError(
        paste0(
          "Invalid hierarchy, names must not match column names in the TT.res slot:\n  \"",
          paste0(hier_names[invalidNames], collapse="\", \""),
          "\""
        )
      ))
    } else {}
    initial_df <- data.frame(
      kRp_df,
      matrix(
        ncol=length(hierarchy),
        dimnames=list(c(), hier_names)
      )
    )
    for (thisCat in hier_names) {
      initial_df[[thisCat]] <- factor(NA, levels=hierarchy[[thisCat]])
    }
    return(initial_df)
  } else {
    return(kRp_df)
  }
} ## end function init_flatHier()


#' S4 Class kRp.flatHier
#'
#' Objects of this class can contain full text corpora in a hierachical structure. It supports both the \code{tm} package's
#' \code{\link[tm]{Corpus}} class and \code{koRpus}' own object classes and stores them in separated slots.
#' 
#' Objects should be created using the \code{\link[tm.plugin.koRpus:readCorpus]{readCorpus}} function.
#' 
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_flatHier(...)} can be used instead of
#' \code{new("kRp.flatHier", ...)}. Whenever possible, stick to
#' \code{\link[tm.plugin.koRpus:readCorpus]{readCorpus}}.
#' 
#' @slot lang A character string, naming the language that is assumed for the tokenized texts in this object.
#' @slot desc A named list of descriptive statistics of the tagged texts.
#' @slot hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#' @slot meta A named list. Can be used to store meta information. Currently, no particular format is defined.
#' @slot raw A list of objects of class \code{\link[tm]{Corpus}}.
#' @slot TT.res A data frame as used for the \code{TT.res} slot in objects of class \code{kRp.taggedText}. In addition to the columns
#'    usually found in those objects, this data frame also has a factor column for each hierarchical category defined (if any).
#' @slot summary A summary data frame for the full corpus.
#' @slot hyphen A named list of objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.
#' @slot readability A named list of objects of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @slot freq.analysis A list with information on the word frequencies of the analyzed text.
#' @slot freq A list with two elements, \code{freq.analysis} and \code{corpus}. \code{freq.analysis} contains the
#'    \code{freq.analysis} slot of a \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}} class object after
#'    \code{\link[tm.plugin.koRpus:freq.analysis]{freq.analysis}} was called, whereas \code{corpus} holds the results of a call to
#'    \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}}.
#' @slot diff A named list of \code{diff} slots of a \code{\link[sylly:kRp.text.trans-class]{kRp.text.trans}} object after
#'    a method like \code{\link[tm.plugin.koRpus:textTransform]{textTransform}} was called.
#' @note There is also \code{\link[tm.plugin.koRpus:kRp.flatHier_get-methods]{getter and setter methods}} for objects of this class.
#' @name kRp.flatHier,-class
#' @aliases kRp.flatHier,-class kRp.flatHier-class
#' @import methods koRpus
#' @importFrom tm as.VCorpus
#' @keywords classes
#' @export kRp_flatHier
#' @exportClass kRp.flatHier
#' @rdname kRp.flatHier
#' @examples
#' \dontrun{
#' # use readCorpus() to create objects of class kRp.flatHier
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
#' }
#' # manual creation
#' emptyCorpus <- kRp_hierarchy()
kRp_flatHier <- setClass("kRp.flatHier",
  representation=representation(
    lang="character",
    desc="list",
    hierarchy="list",
    meta="list",
    raw="list",
    TT.res="data.frame",
    summary="data.frame",
    hyphen="list",
    readability="list",
    TTR="list",
    freq="list",
    diff="list"
  ),
  prototype=prototype(
    lang=character(),
    desc=list(),
    hierarchy=list(),
    meta=list(),
    raw=list(),
    TT.res=init_flatHier_TT.res(),
    summary=data.frame(),
    hyphen=list(),
    readability=list(),
    TTR=list(),
    freq=list(freq.analysis=list(), corpus=kRp_corp_freq()),
    diff=list()
  )
)

setValidity("kRp.flatHier", function(object){
  # TODO: check if hierarchy is resembled by rows in tagged object
    hierarchy <- slot(object, "hierarchy")
    raw <- slot(object, "raw")
    TT.res <- slot(object, "TT.res")
    hyphen <- slot(object, "hyphen")
    readability <- slot(object, "readability")
    TTR <- slot(object, "TTR")
    freq <- slot(object, "freq")

    freq.names <- names(freq)
    if(!all(c("freq.analysis", "corpus") %in% freq.names)){
      stop(simpleError("Invalid object: Slot \"freq\" needs to have two entries called \"freq.analysis\" and \"corpus\"!"))
    } else {}
    freq.freq.analysis <- freq[["freq.analysis"]]
    if(!inherits(freq[["corpus"]], "kRp.corp.freq")){
      stop(simpleError(paste0("Invalid object: Item \"corpus\" in slot \"freq\" must have entries inheriting from class kRp.corp.freq!")))
    } else {}

    TT.res.names <- colnames(TT.res)
    standard.TT.res.names <- colnames(koRpus::taggedText(koRpus::kRp_tagged()))
    missingCols <- standard.TT.res.names[!standard.TT.res.names %in% TT.res.names]
    if(length(missingCols) > 0){
      warning(
        paste0(
          "Invalid object: Missing columns in slot \"TT.res\":\n  ",
          paste0(missingCols, collapse=", ")
        ),
        call.=FALSE)
    } else {}

    classObj <- list(
      "Corpus"=list(name="raw", obj=raw),
      "kRp.hyphen"=list(name="hyphen", obj=hyphen),
      "kRp.readability"=list(name="readability", obj=readability),
      "kRp.TTR"=list(name="TTR", obj=TTR)
    )
    for (thisClassObj in names(classObj)) {
      if(!identical(classObj[[thisClassObj]][["obj"]], list()) && !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))){
        stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
      } else {}
    }

    return(TRUE)
})


## method flatHier2tagged()
# returns a list of kRp.tagged objects from an object of class kRp.flatHier
# element names are the doc_id
setGeneric("flatHier2tagged", function(obj) standardGeneric("flatHier2tagged"))
setMethod("flatHier2tagged",
  signature=signature(obj="kRp.flatHier"),
  function(obj){
    tt_desc <- describe(obj)
    tt_lang <- language(obj)
    tt_diff <- diffText(obj)
    tt_tagged <- taggedText(obj)
    tt_list <- split(tt_tagged, tt_tagged[["doc_id"]])
    if(all(
      length(tt_diff) > 0,
      identical(names(tt_desc), names(tt_diff))
    )){
      result <- lapply(
        names(tt_list),
        function(thisText){
          kRp_txt_trans(
            lang=tt_lang,
            desc=tt_desc[[thisText]],
            TT.res=tt_list[[thisText]],
            diff=tt_desc[[thisText]]
          )
        }
      )
    } else {
      result <- lapply(
        names(tt_list),
        function(thisText){
          kRp_tagged(
            lang=tt_lang,
            desc=tt_desc[[thisText]],
            TT.res=tt_list[[thisText]]
          )
        }
      )
    }
    names(result) <- names(tt_desc)
    return(result)
  }
) ## end method flatHier2tagged()
