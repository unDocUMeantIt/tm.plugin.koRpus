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
#' inside the given \code{words} object (using \code{lapply}).
#' 
#' @param words An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#'    \code{\link[tm.plugin.koRpus]{kRp.sourcesCorpus-class}} or
#'    \code{\link[tm.plugin.koRpus]{kRp.topicCorpus-class}}.
#' @param ... options to pass through to \code{\link[koRpus:hyphen]{hyphen}}.
#' @export
#' @docType methods
#' @aliases hyphen,kRp.corpus-method
#' @rdname hyphen-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- hyphen(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
setMethod("hyphen", signature(words="kRp.corpus"), function(words, ...){
    slot(words, "hyphen") <- lapply(slot(words, "tagged"), function(thisText){
      hyphen(thisText, ...)
    })
    return(words)
  }
)

#' @aliases hyphen,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname hyphen-methods
#' @export
setMethod("hyphen", signature(words="kRp.sourcesCorpus"), function(words, ...){
    all.corpora <- slot(words, "sources")

    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- hyphen(all.corpora[[thisCorpus]], ...)
    }
    slot(words, "sources") <- all.corpora

    return(words)
  }
)

#' @aliases hyphen,kRp.topicCorpus-method
#' @docType methods
#' @rdname hyphen-methods
#' @export
setMethod("hyphen", signature(words="kRp.topicCorpus"), function(words, ...){
    all.topics <- slot(words, "topics")

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- hyphen(all.topics[[thisTopic]], ...)
    }
    slot(words, "topics") <- all.topics

    return(words)
  }
)
