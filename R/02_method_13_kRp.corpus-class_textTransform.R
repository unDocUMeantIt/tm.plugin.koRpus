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


#' Apply textTransform() to all texts in kRp.hierarchy objects
#' 
#' This method calls \code{\link[koRpus:textTransform]{textTransform}} on all tagged text objects
#' inside the given \code{txt} object (using \code{lapply}).
#' 
#' @param txt An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:textTransform]{textTransform}}.
#' @return An object of the same class as \code{txt}.
#' @importFrom parallel mclapply
#' @importFrom koRpus textTransform
#' @export
#' @docType methods
#' @aliases textTransform,kRp.hierarchy-method
#' @rdname textTransform
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
#' myCorpus <- textTransform(myCorpus, scheme="minor")
#' }
#' @include 01_class_01_kRp.hierarchy.R
setMethod("textTransform", signature(txt="kRp.hierarchy"), function(txt, mc.cores=getOption("mc.cores", 1L), ...){
    if(corpusLevel(txt) > 0){
      corpusChildren(txt) <- lapply(corpusChildren(txt), textTransform, mc.cores=mc.cores, ...)
    } else {
      corpusTagged(txt) <- mclapply(corpusTagged(txt), function(thisText){
        textTransform(thisText, ...)
      }, mc.cores=mc.cores)
    }

    return(txt)
  }
)
