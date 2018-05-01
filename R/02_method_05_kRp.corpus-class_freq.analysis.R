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


#' Apply freq.analysis() to all texts in kRp.hierarchy objects
#' 
#' This method calls \code{\link[koRpus:freq.analysis]{freq.analysis}} on all tagged text objects
#' inside the given \code{txt.file} object (using \code{lapply}).
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#' @return An object of the same class as \code{txt.file}.
#' @importFrom parallel mclapply
#' @importFrom koRpus freq.analysis
#' @export
#' @docType methods
#' @aliases freq.analysis,kRp.hierarchy-method
#' @rdname freq.analysis-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- freq.analysis(myTexts)
#' }
#' @include 01_class_01_kRp.hierarchy.R
setMethod("freq.analysis", signature(txt.file="kRp.hierarchy"), function(txt.file, mc.cores=getOption("mc.cores", 1L), ...){
    if(corpusLevel(txt.file) > 0){
      corpusChildren(txt.file) <- lapply(corpusChildren(txt.file), freq.analysis, mc.cores=mc.cores, ...)
    } else {
      corpusTagged(txt.file) <- mclapply(corpusTagged(txt.file), function(thisText){
        freq.analysis(thisText, ...)
      }, mc.cores=mc.cores)
    }

    return(txt.file)
  }
)
