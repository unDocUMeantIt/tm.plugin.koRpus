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
#' inside the given \code{txt.file} object (using \code{lapply}).
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#'    \code{\link[tm.plugin.koRpus]{kRp.sourcesCorpus-class}} or
#'    \code{\link[tm.plugin.koRpus]{kRp.topicCorpus-class}}.
#' @param summary Logical, determines if the \code{summary} slot should automatically be
#'    updated by calling \code{\link[tm.plugin.koRpus:summary]{summary}} on the result.
#' @param ... options to pass through to \code{\link[koRpus:readability]{readability}}.
#' @export
#' @docType methods
#' @aliases readability,kRp.corpus-method
#' @rdname readability-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- readability(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
#' @export
setMethod("readability", signature(txt.file="kRp.corpus"), function(txt.file, summary=TRUE, ...){
    corpusReadability(txt.file) <- lapply(names(corpusTagged(txt.file)), function(thisText){
      if(thisText %in% names(corpusHyphen(txt.file)) & is.null(list(...)[["hyphen"]])){
        # we probably need to drop one of two hyphen arguments of
        # readability was called from one of the wrapper functions
        default <- list(txt.file=corpusTagged(txt.file)[[thisText]], ...)
        args <- modifyList(default, list(hyphen=corpusHyphen(txt.file)[[thisText]]))
        rdb <- do.call(readability, args)
      } else {
        rdb <- readability(corpusTagged(txt.file)[[thisText]], ...)
      }
      return(rdb)
    })
    # store meta-information on the maximum of available indices.
    # a mere summary() will simply omit NA values which can later cause
    # problems when we want to aggregate all summaries into one data.frame
    corpusMeta(txt.file, "readability") <- list(index=c())
    corpusMeta(txt.file, "readability")[["index"]] <- sort(
      unique(
        unlist(lapply(corpusReadability(txt.file), function(thisText){
            names(summary(thisText, flat=TRUE))
          })
        )
      )
    )
    names(corpusReadability(txt.file)) <- names(corpusTagged(txt.file))
    
    if(isTRUE(summary)){
      txt.file <- summary(txt.file)
    } else {}
    
    return(txt.file)
  }
)

#' @aliases readability,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname readability-methods
#' @export
setMethod("readability", signature(txt.file="kRp.sourcesCorpus"), function(txt.file, summary=TRUE, ...){
    all.corpora <- slot(txt.file, "sources")

    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- readability(all.corpora[[thisCorpus]], summary=summary, ...)
    }
    slot(txt.file, "sources") <- all.corpora

    if(isTRUE(summary)){
      txt.file <- summary(txt.file)
    } else {}

    return(txt.file)
  }
)

#' @aliases readability,kRp.topicCorpus-method
#' @docType methods
#' @rdname readability-methods
#' @export
setMethod("readability", signature(txt.file="kRp.topicCorpus"), function(txt.file, summary=TRUE, ...){
    all.topics <- slot(txt.file, "topics")

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- readability(all.topics[[thisTopic]], summary=summary, ...)
    }
    slot(txt.file, "topics") <- all.topics

    if(isTRUE(summary)){
      txt.file <- summary(txt.file)
    } else {}

    return(txt.file)
  }
)
