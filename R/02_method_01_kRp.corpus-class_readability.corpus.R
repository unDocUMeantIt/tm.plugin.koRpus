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

#' Apply readability() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:readability]{readability}} on all tagged text objects
#' inside the given \code{corpus} object (using \code{lapply}).
#' 
#' @param corpus An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}}.
#' @param ... options to pass through to \code{\link[koRpus:readability]{readability}}.
#' @export
#' @docType methods
#' @aliases readability.corpus,kRp.corpus-method
#' @rdname readability.corpus-methods
#' @examples
#' \dontrun{
#' myTexts <- kRp.Corpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- readability.corpus(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
#' @export
setGeneric("readability.corpus", function(corpus, ...) standardGeneric("readability.corpus"))

#' @export
#' @rdname readability.corpus-methods
setMethod("readability.corpus", signature(corpus="kRp.corpus"), function(corpus, ...){
    slot(corpus, "readability") <- lapply(names(slot(corpus, "tagged")), function(thisText){
      if(thisText %in% names(slot(corpus, "hyphen"))){
        readability(slot(corpus, "tagged")[[thisText]], hyphen=slot(corpus, "hyphen")[[thisText]], ...)
      } else {
        readability(slot(corpus, "tagged")[[thisText]], ...)
      }
    })
    names(slot(corpus, "readability")) <- names(slot(corpus, "tagged"))
    return(corpus)
  }
)
