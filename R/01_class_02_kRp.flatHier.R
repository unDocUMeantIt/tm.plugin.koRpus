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
#' @slot hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#' @slot meta A named list. Can be used to store meta information. Currently, no particular format is defined.
#' @slot raw A list of objects of class \code{\link[tm]{Corpus}}.
#' @slot TT.res A data frame as used for the \code{TT.res} slot in objects of class \code{kRp.taggedText}. In addition to the columns
#'    usually found in those objects, this data frame also has a factor column for each hierarchical category defined (if any).
#' @note There is also \code{\link[tm.plugin.koRpus:kRp.flatHier_get-methods]{getter and setter methods}} for objects of this class.
#' @name kRp.flatHier,-class
#' @aliases kRp.flatHier,-class kRp.flatHier-class
#' @import methods koRpus
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
    hierarchy="list",
    meta="list",
    raw="list",
    TT.res="data.frame"
  ),
  prototype=prototype(
    hierarchy=list(),
    meta=list(),
    raw=list(),
    TT.res=koRpus::taggedText(koRpus::kRp_tagged())
  )
)

setValidity("kRp.flatHier", function(object){
  # TODO: check if hierarchy is resembled by rows in tagged object
    hierarchy <- slot(object, "hierarchy")
    raw <- slot(object, "raw")
    TT.res <- slot(object, "TT.res")
    TT.res.names <- colnames(TT.res)
    standard.TT.res.names <- colnames(koRpus::taggedText(koRpus::kRp_tagged()))

    classObj <- list(
      "Corpus"=list(name="raw", obj=raw)
    )
    for (thisClassObj in names(classObj)) {
      if(!identical(classObj[[thisClassObj]][["obj"]], list()) && !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))){
        stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
      } else {}
    }

    missingCols <- standard.TT.res.names[!standard.TT.res.names %in% TT.res.names]
    if(length(missingCols) > 0){
      warning(
        paste0(
          "Invalid object: Missing columns in slot \"TT.res\":\n  ",
          paste0(missingCols, collapse=", ")
        ),
        call.=FALSE)
    } else {}

    return(TRUE)
})
