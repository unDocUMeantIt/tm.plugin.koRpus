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


#' Apply read.corp.custom() to all texts in kRp.hierarchy objects
#' 
#' This method calls \code{\link[koRpus:read.corp.custom]{read.corp.custom}} on all tagged text objects
#' inside the given \code{corpus} object (using \code{lapply}).
#' 
#' @param corpus An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:read.corp.custom]{read.corp.custom}}.
#' @return An object of the same class as \code{corpus}.
#' @export
#' @importFrom parallel mclapply
#' @importFrom koRpus read.corp.custom
#' @docType methods
#' @aliases read.corp.custom,kRp.hierarchy-method
#' @rdname read.corp.custom
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
#' # this will call read.corp.custom() recursively
#' myCorpus <- read.corp.custom(myCorpus)
#' }
#' @include 01_class_01_kRp.hierarchy.R
setMethod("read.corp.custom", signature(corpus="kRp.hierarchy"), function(corpus, mc.cores=getOption("mc.cores", 1L), ...){
    if(corpusLevel(corpus) > 0){
      corpusChildren(corpus) <- lapply(corpusChildren(corpus), read.corp.custom, mc.cores=mc.cores, ...)
      corpusFreq(corpus)[["corpus"]] <- read.corp.custom(unlist(lapply(corpusChildren(corpus, level=0), corpusTagged)), ...)
    } else {
      # individual tests
      corpusFreq(corpus)[["texts"]] <- mclapply(corpusTagged(corpus), function(thisText){
        read.corp.custom(thisText, ...)
      }, mc.cores=mc.cores)
      # analysis on full corpus
      corpusFreq(corpus)[["corpus"]] <- read.corp.custom(corpusTagged(corpus), ...)
    }

    return(corpus)
  }
)
