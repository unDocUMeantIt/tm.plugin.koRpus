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


#' Apply freq.analysis() to all texts in kRp.flatHier objects
#' 
#' This method calls \code{\link[koRpus:freq.analysis]{freq.analysis}} on all tagged text objects
#' inside the given \code{txt.file} object.
#' 
#' If \cose{corp.freq} was not specified but a valid object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}
#' is found in the \code{freq} slot of \code{txt.file}, it is used automatically. That is the case if you called
#' \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}} on the object previously.
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus:kRp.flatHier-class]{kRp.flatHier}}.
#' @param ... options to pass through to \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#' @return An object of the same class as \code{txt.file}.
#' @importFrom koRpus freq.analysis
#' @export
#' @docType methods
#' @aliases freq.analysis,kRp.flatHier-method
#' @rdname freq.analysis
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
#' # this will call read.corp.custom()
#' myCorpus <- read.corp.custom(myCorpus)
#' myCorpus <- freq.analysis(myCorpus)
#' }
#' @include 01_class_01_kRp.flatHier.R
setMethod("freq.analysis", signature(txt.file="kRp.flatHier"), function(txt.file, ...){
    tagged_large <- kRp_tagged(
      lang=language(txt.file),
      TT.res=taggedText(txt.file)
    )

    use_default_args <- TRUE
    if(is.null(list(...)[["corp.freq"]])){
      if(is(corpusFreq(txt.file)[["corpus"]], "kRp.corp.freq")){
        if(nrow(slot(corpusFreq(txt.file)[["corpus"]], "words")) > 1){
          default <- list(txt.file=tagged_large, ...)
          args <- modifyList(default, list(corp.freq=corpusFreq(txt.file)[["corpus"]]))
          use_default_args <- FALSE
          freq_ananlysis_results <- do.call(freq.analysis, args)
        } else {}
      } else {}
    } else {}

    if(isTRUE(use_default_args)){
      freq_ananlysis_results <- freq.analysis(tagged_large, ...)
    } else {}

    taggedText(txt.file) <- taggedText(freq_ananlysis_results)
    corpusMeta(txt.file, meta="freq.analysis") <- describe(freq_ananlysis_results)
    corpusFreq(txt.file)[["freq.analysis"]] <- slot(freq_ananlysis_results, "freq.analysis")
    return(txt.file)
  }
)
