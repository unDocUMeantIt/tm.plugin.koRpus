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


#' Apply freq.analysis() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:freq.analysis]{freq.analysis}} on all tagged text objects
#' inside the given \code{txt.file} object (using \code{lapply}).
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#'    \code{\link[tm.plugin.koRpus]{kRp.sourcesCorpus-class}} or
#'    \code{\link[tm.plugin.koRpus]{kRp.topicCorpus-class}}.
#' @param ... options to pass through to \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#' @export
#' @docType methods
#' @aliases freq.analysis,kRp.corpus-method
#' @rdname freq.analysis-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- freq.analysis(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
setMethod("freq.analysis", signature(txt.file="kRp.corpus"), function(txt.file, ...){
    slot(txt.file, "tagged") <- lapply(slot(txt.file, "tagged"), function(thisText){
      freq.analysis(thisText, ...)
    })
    return(txt.file)
  }
)

#' @aliases freq.analysis,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname freq.analysis-methods
#' @export
setMethod("freq.analysis", signature(txt.file="kRp.sourcesCorpus"), function(txt.file, ...){
    all.corpora <- slot(txt.file, "sources")

    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- freq.analysis(all.corpora[[thisCorpus]], ...)
    }
    slot(txt.file, "sources") <- all.corpora

    return(txt.file)
  }
)

#' @aliases freq.analysis,kRp.topicCorpus-method
#' @docType methods
#' @rdname freq.analysis-methods
#' @export
setMethod("freq.analysis", signature(txt.file="kRp.topicCorpus"), function(txt.file, ...){
    all.topics <- slot(txt.file, "topics")

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- freq.analysis(all.topics[[thisTopic]], ...)
    }
    slot(txt.file, "topics") <- all.topics

    return(txt.file)
  }
)
