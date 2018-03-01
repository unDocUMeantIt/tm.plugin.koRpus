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


#' Apply hyphen() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[sylly:hyphen]{hyphen}} on all tagged text objects
#' inside the given \code{words} object (using \code{lapply}).
#' 
#' @param words An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}},
#'    \code{\link[tm.plugin.koRpus:kRp.sourcesCorpus-class]{kRp.sourcesCorpus}} or
#'    \code{\link[tm.plugin.koRpus:kRp.topicCorpus-class]{kRp.topicCorpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param quiet Logical, if \code{FALSE} shows a status bar for the hyphenation process of each text.
#' @param ... options to pass through to \code{\link[sylly:hyphen]{hyphen}}.
#' @return An object of the same class as \code{words}.
#' @importFrom parallel mclapply
#' @importFrom sylly hyphen
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
setMethod("hyphen", signature(words="kRp.corpus"), function(words, mc.cores=getOption("mc.cores", 1L), quiet=TRUE, ...){
    corpusHyphen(words) <- mclapply(corpusTagged(words), function(thisText){
      hyphen(thisText, quiet=quiet, ...)
    }, mc.cores=mc.cores)
    return(words)
  }
)

#' @aliases hyphen,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname hyphen-methods
#' @export
setMethod("hyphen", signature(words="kRp.sourcesCorpus"), function(words, mc.cores=getOption("mc.cores", 1L), quiet=TRUE, ...){
    all.corpora <- corpusSources(words)

    for (thisCorpus in names(all.corpora)){
      all.corpora[[thisCorpus]] <- hyphen(all.corpora[[thisCorpus]], mc.cores=mc.cores, quiet=quiet, ...)
    }
    corpusSources(words) <- all.corpora

    return(words)
  }
)

#' @aliases hyphen,kRp.topicCorpus-method
#' @docType methods
#' @rdname hyphen-methods
#' @export
setMethod("hyphen", signature(words="kRp.topicCorpus"), function(words, mc.cores=getOption("mc.cores", 1L), quiet=TRUE, ...){
    all.topics <- corpusTopics(words)

    for (thisTopic in names(all.topics)){
      all.topics[[thisTopic]] <- hyphen(all.topics[[thisTopic]], mc.cores=mc.cores, quiet=quiet, ...)
    }
    corpusTopics(words) <- all.topics

    return(words)
  }
)
