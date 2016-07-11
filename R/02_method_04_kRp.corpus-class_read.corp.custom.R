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


#' Apply read.corp.custom() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:read.corp.custom]{read.corp.custom}} on all tagged text objects
#' inside the given \code{corpus} object (using \code{lapply}).
#' 
#' @param corpus An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#'    \code{\link[tm.plugin.koRpus]{kRp.sourcesCorpus-class}} or
#'    \code{\link[tm.plugin.koRpus]{kRp.topicCorpus-class}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:read.corp.custom]{read.corp.custom}}.
#' @return An object of the same class as \code{corpus}.
#' @export
#' @docType methods
#' @aliases read.corp.custom read.corp.custom,kRp.corpus-method
#' @rdname read.corp.custom-methods
#' @examples
#' \dontrun{
#' myBasePath <- file.path("/home","me","textCorpus")
#' # analyse a single corpus
#' myTexts <- simpleCorpus(dir=myBasePath)
#' myTexts <- read.corp.custom(myTexts)
#' 
#' # you can also analyse multiple corpora from different sources simultaneously
#' # the following would assume that below 'myBasePath', there are subfolders
#' # named "Wikipedia" and Journal of Applied Geschwurbel"
#' mySources <- c(
#'    wp="Wikipedia",
#'    jg="Journal of Applied Geschwurbel"
#'  )
#' mySourcesTexts <- sourcesCorpus(
#'   path=myBasePath,
#'   sources=mySources
#' )
#' # this will also call read.corp.custom() recursively on
#' # the single text level
#' mySourcesTexts <- read.corp.custom(mySourcesTexts)
#' 
#' # and you might have guessed, you can also add a topic level
#' # with the following two vectors you would describe your
#' # data structure as two subfolders called "proc" and "gesw"
#' # below myBasePath, and two further subfolders named "Wikipedia"
#' # and Journal of Applied Geschwurbel" below each of the topic folders,
#' # containing the actual texts
#' myTopicPaths <- c(
#'   procrastination=file.path(myBasePath, "proc"),
#'   geschwurbel=file.path(myBasePath, "gesw")
#' )
#' mySources <- c(
#'    wp="Wikipedia",
#'    jg="Journal of Applied Geschwurbel"
#'  )
#' myTopicTexts <- topicCorpus(
#'   paths=myTopicPaths,
#'   sources=mySources
#' )
#' # this will also call read.corp.custom() recursively on
#' # the source and single text level
#' myTopicTexts <- read.corp.custom(myTopicTexts)
#' 
#' }
#' @include 01_class_01_kRp.corpus.R
#' @import koRpus
setMethod("read.corp.custom", signature(corpus="kRp.corpus"), function(corpus, mc.cores=getOption("mc.cores", 1L), ...){
    # individual tests
    corpusFreq(corpus)[["texts"]] <- mclapply(corpusTagged(corpus), function(thisText){
      read.corp.custom(thisText, ...)
    }, mc.cores=mc.cores)
    # analysis on full corpus
    corpusFreq(corpus)[["corpus"]] <- read.corp.custom(corpusTagged(corpus), ...)
    return(corpus)
  }
)

#' @aliases read.corp.custom,kRp.sourcesCorpus-method
#' @docType methods
#' @rdname read.corp.custom-methods
#' @export
setMethod("read.corp.custom", signature(corpus="kRp.sourcesCorpus"), function(corpus, mc.cores=getOption("mc.cores", 1L), ...){
    all.corpora <- slot(corpus, "sources")

    all.tagged <- list()
    # iterate through all texts indivudually
    for (thisCorpus in names(all.corpora)){
      all.tagged <- append(all.tagged, slot(all.corpora[[thisCorpus]], "tagged"))
      all.corpora[[thisCorpus]] <- read.corp.custom(all.corpora[[thisCorpus]], mc.cores=mc.cores, ...)
    }
    slot(corpus, "sources") <- all.corpora
    # analysis on full corpus
    slot(corpus, "freq") <- read.corp.custom(all.tagged, ...)

    return(corpus)
  }
)

#' @aliases read.corp.custom,kRp.topicCorpus-method
#' @docType methods
#' @rdname read.corp.custom-methods
#' @export
setMethod("read.corp.custom", signature(corpus="kRp.topicCorpus"), function(corpus, mc.cores=getOption("mc.cores", 1L), ...){
    all.topics <- slot(corpus, "topics")

    all.tagged <- list()
    # iterate through all texts indivudually
    for (thisTopic in names(all.topics)){
      sub.corpora <- slot(all.topics[[thisTopic]], "sources")
      for (thisCorpus in names(sub.corpora)){
        all.tagged <- append(all.tagged, slot(sub.corpora[[thisCorpus]], "tagged"))
      }
      all.topics[[thisTopic]] <- read.corp.custom(all.topics[[thisTopic]], mc.cores=mc.cores, ...)
    }
    slot(corpus, "topics") <- all.topics
    # analysis on full corpus
    slot(corpus, "freq") <- read.corp.custom(all.tagged, ...)

    return(corpus)
  }
)
