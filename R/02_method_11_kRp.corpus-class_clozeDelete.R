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


#' Apply clozeDelete() to all texts in kRp.hierarchy objects
#' 
#' This method calls \code{\link[koRpus:clozeDelete]{clozeDelete}} on all tagged text objects
#' inside the given \code{obj} object (using \code{lapply}).
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:clozeDelete]{clozeDelete}}.
#' @return An object of the same class as \code{obj}.
#' @importFrom parallel mclapply
#' @importFrom koRpus clozeDelete
#' @export
#' @docType methods
#' @aliases clozeDelete,kRp.hierarchy-method
#' @rdname clozeDelete
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
#' # remove all punctuation
#' myCorpus <- clozeDelete(myCorpus)
#' }
#' @include 01_class_01_kRp.hierarchy.R
setMethod("clozeDelete", signature(obj="kRp.hierarchy"), function(obj, mc.cores=getOption("mc.cores", 1L), ...){
    if(corpusLevel(obj) > 0){
      corpusChildren(obj) <- lapply(corpusChildren(obj), clozeDelete, mc.cores=mc.cores, ...)
    } else {
      corpusTagged(obj) <- mclapply(corpusTagged(obj), function(thisText){
        clozeDelete(thisText, ...)
      }, mc.cores=mc.cores)
    }

    return(obj)
  }
)