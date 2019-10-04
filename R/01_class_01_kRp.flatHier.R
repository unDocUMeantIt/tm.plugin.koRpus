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
#' @slot desc A list of descriptive statistics of the tagged texts.
#' @slot hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#' @slot meta A named list. Can be used to store meta information. Currently, no particular format is defined.
#' @slot raw A list of objects of class \code{\link[tm]{Corpus}}.
#' @slot TT.res A data frame as used for the \code{TT.res} slot in objects of class \code{kRp.taggedText}. In addition to the columns
#'    usually found in those objects, this data frame also has a factor column for each hierarchical category defined (if any).
#' @slot summary A summary data frame for the full corpus.
#' @slot hyphen A list of objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.
#' @slot readability A list of objects of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @note There is also \code{\link[tm.plugin.koRpus:kRp.flatHier_get-methods]{getter and setter methods}} for objects of this class.
#' @name kRp.flatHier,-class
#' @aliases kRp.flatHier,-class kRp.flatHier-class
#' @import methods koRpus
#' @import tm as.VCorpus
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
    freq="list"
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
    freq=list(texts=list(), corpus=kRp_corp_freq())
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
    if(!all(c("texts", "corpus") %in% freq.names)){
      stop(simpleError("Invalid object: Slot \"freq\" needs to have two entries called \"texts\" and \"corpus\"!"))
    } else {}
    freq.texts <- freq[["texts"]]
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
      "kRp.TTR"=list(name="TTR", obj=TTR),
      "kRp.corp.freq"=list(name="freq.texts", obj=freq.texts)
    )
    for (thisClassObj in names(classObj)) {
      if(!identical(classObj[[thisClassObj]][["obj"]], list()) && !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))){
        stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
      } else {}
    }

    return(TRUE)
})


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


## method append_flatHier()
# appends the content of a kRp.tagged object to a kRp.flatHier object during corpus import
# omitting lang as that should be set globally
# - obj: the kRp.flatHier object
# - TT.res: the TT.res data frame to append
# - desc: the desc slot to add
# - all_files: the data frame of the same name generated by corpus_files()
setGeneric("append_flatHier", function(obj, TT.res, desc, all_files) standardGeneric("append_flatHier"))
setMethod("append_flatHier",
  signature=signature(obj="kRp.flatHier"),
  function (obj, TT.res, desc, all_files){
    obj_df <- taggedText(obj)
    if(!identical(names(obj_df), names(TT.res))){
      missingCols <- names(obj_df)[!names(obj_df) %in% names(TT.res)]
      for (thisCat in missingCols) {
        TT.res[[thisCat]] <- factor(
          all_files[all_files[["doc_id"]] == desc[["doc_id"]], thisCat],
          levels=levels(obj_df[[thisCat]])
        )
      }
    } else {}
    taggedText(obj) <- rbind(obj_df, TT.res)
    describe(obj)[[desc[["doc_id"]]]] <- desc
    return(obj)
  }
) ## end method append_flatHier()
