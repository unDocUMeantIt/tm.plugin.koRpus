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


#' Apply lex.div() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:lex.div]{lex.div}} on all tagged text objects
#' inside the given \code{txt} object (using \code{lapply}).
#' 
#' @param txt An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#'    \code{\link[tm.plugin.koRpus]{kRp.sourcesCorpus-class}} or
#'    \code{\link[tm.plugin.koRpus]{kRp.topicCorpus-class}}.
#' @param summary Logical, determines if the \code{summary} slot should automatically be
#'    updated by calling \code{\link[tm.plugin.koRpus:summary]{summary}} on the result.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:lex.div]{lex.div}}.
#' @return An object of the same class as \code{txt}.
#' @export
#' @docType methods
#' @aliases lex.div,kRp.corpus-method
#' @rdname lex.div-methods
#' @examples
#' \dontrun{
#' myTexts <- simpleCorpus(dir=file.path("/home","me","textCorpus"))
#' myTexts <- lex.div(myTexts)
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
setMethod("lex.div", signature(txt="kRp.corpus"), function(txt, summary=TRUE, mc.cores=getOption("mc.cores", 1L), ...){
    corpusTTR(txt) <- mclapply(corpusTagged(txt), function(thisText){
      lex.div(thisText, ...)
    }, mc.cores=mc.cores)
    # store meta-information on the maximum of available indices.
    # a mere summary() will simply omit NA values which can later cause
    # problems when we want to aggregate all summaries into one data.frame
    corpusMeta(txt, "TTR") <- list(index=c())
    corpusMeta(txt, "TTR")[["index"]] <- sort(
      unique(
        unlist(mclapply(corpusTTR(txt), function(thisText){
            names(summary(thisText, flat=TRUE))
          }, mc.cores=mc.cores)
        )
      )
    )
    
    if(isTRUE(summary)){
      txt <- summary(txt)
    } else {}

    return(txt)
  }
)

#' @aliases lex.div,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname lex.div-methods
#' @export
setMethod("lex.div", signature(txt="kRp.sourcesCorpus"), function(txt, summary=TRUE, mc.cores=getOption("mc.cores", 1L), ...){
    all.corpora <- corpusSources(txt)

    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- lex.div(all.corpora[[thisCorpus]], summary=summary, mc.cores=mc.cores, ...)
    }
    corpusSources(txt) <- all.corpora

    if(isTRUE(summary)){
      txt <- summary(txt)
    } else {}

    return(txt)
  }
)

#' @aliases lex.div,kRp.topicCorpus-method
#' @docType methods
#' @rdname lex.div-methods
#' @export
setMethod("lex.div", signature(txt="kRp.topicCorpus"), function(txt, summary=TRUE, mc.cores=getOption("mc.cores", 1L), ...){
    all.topics <- corpusTopics(txt)

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- lex.div(all.topics[[thisTopic]], summary=summary, mc.cores=mc.cores, ...)
    }
    corpusTopics(txt) <- all.topics

    if(isTRUE(summary)){
      txt <- summary(txt)
    } else {}

    return(txt)
  }
)
