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


#' S4 Class kRp.corpus
#'
#' Objects of this class can contain multiple texts simultaneously. It supports both the \code{tm} package's
#' \code{\link[tm]{Corpus}} class and \code{koRpus}' own object classes and stores them in separated slots.
#' 
#' Objects should be created using the \code{\link[tm.plugin.koRpus:simpleCorpus]{simpleCorpus}} function.
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_corpus(...)} can be used instead of
#' \code{new("kRp.corpus", ...)}. Whenever possible, stick to
#' \code{\link[tm.plugin.koRpus:simpleCorpus]{simpleCorpus}}.
#' 
#' @slot summary A summary data frame for the full corpus.
#' @slot meta A named list. Can be used to store meta information. Currently, no particular format is defined.
#' @slot raw A list of objects of class \code{\link[tm]{Corpus}}.
#' @slot tagged A list of objects of class \code{kRp.taggedText} (a class union for tagged text objects).
#' @slot hyphen A list of objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.
#' @slot TTR A list of objects of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
#' @slot readability A list of objects of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.
#' @slot freq A list with two elements, \code{texts} and \code{corpus}. Both hold objects of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}},
#'    where \code{texts} is a list of these objects (one for each text), and \code{corpus} is a single object for the full corpus.
#' @note There is also \code{\link[tm.plugin.koRpus:kRp.corpus_get-methods]{getter and setter methods}} for objects of this class.
#' @name kRp.corpus,-class
#' @aliases kRp.corpus,-class kRp.corpus-class
#' @import methods koRpus
#' @keywords classes
#' @export kRp_corpus
#' @exportClass kRp.corpus
#' @rdname kRp.corpus-class
kRp_corpus <- setClass("kRp.corpus",
    representation=representation(
    summary="data.frame",
    meta="list",
    raw="list",
    tagged="list",
    hyphen="list",
    TTR="list",
    readability="list",
    freq="list"),
  prototype=prototype(
    summary=data.frame(),
    meta=list(),
    raw=list(),
    tagged=list(),
    hyphen=list(),
    TTR=list(),
    readability=list(),
    freq=list(texts=list(), corpus=kRp_corp_freq())
  )
)

setValidity("kRp.corpus", function(object){
    meta <- slot(object, "meta")
    raw <- slot(object, "raw")
    tagged <- slot(object, "tagged")
    hyphen <- slot(object, "hyphen")
    TTR <- slot(object, "TTR")
    readability <- slot(object, "readability")
    freq <- slot(object, "freq")

    if(!is.list(freq)){
      stop(simpleError("Invalid object: Slot \"freq\" must be a list!"))
    } else {}

    freq.names <- names(freq)
    
    if(!all(c("texts", "corpus") %in% freq.names)){
      stop(simpleError("Invalid object: Slot \"freq\" needs to have two entries called \"texts\" and \"corpus\"!"))
    } else {}

    freq.texts <- freq[["texts"]]
    freq.corpus <- freq[["corpus"]]

    if(!inherits(freq.corpus, "kRp.corp.freq")){
      stop(simpleError("Invalid object: Element \"corpus\" in slot \"freq\" must be of class kRp.corp.freq!"))
    } else {}

    classObj <- list(
      "kRp.taggedText"=list(name="tagged", obj=tagged),
      "Corpus"=list(name="raw", obj=raw),
      "kRp.hyphen"=list(name="hyphen", obj=hyphen),
      "kRp.TTR"=list(name="TTR", obj=TTR),
      "kRp.readability"=list(name="readability", obj=readability),
      "kRp.corp.freq"=list(name="freq.texts", obj=freq.texts)
    )
    for (thisClassObj in names(classObj)) {
      if(!identical(classObj[[thisClassObj]][["obj"]], list()) && !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))){
        message(thisClassObj)
        stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
      } else {}
    }

    return(TRUE)
})
