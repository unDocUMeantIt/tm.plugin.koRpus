# Copyright 2015 Meik Michalke <meik.michalke@hhu.de>
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


#' Apply hyphen() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:hyphen]{hyphen}} on all tagged text objects
#' inside the given \code{corpus} object (using \code{lapply}).
#' 
#' @param corpus An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}}.
#' @param ... options to pass through to \code{\link[koRpus:hyphen]{hyphen}}.
#' @export
#' @docType methods
#' @aliases hyphen.corpus,kRp.corpus-method
#' @rdname hyphen.corpus-methods
#' @examples
#' \dontrun{
#' myTexts <- kRp.Corpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- hyphen.corpus(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
#' @export
setGeneric("hyphen.corpus", function(corpus, ...) standardGeneric("hyphen.corpus"))

#' @export
#' @rdname hyphen.corpus-methods
setMethod("hyphen.corpus", signature(corpus="kRp.corpus"), function(corpus, ...){
    slot(corpus, "hyphen") <- lapply(slot(corpus, "tagged"), function(thisText){
      hyphen(thisText, ...)
    })
    return(corpus)
  }
)
