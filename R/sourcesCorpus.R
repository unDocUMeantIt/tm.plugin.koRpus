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

#' Function to create kRp.sourcesCorpus objects from directory or object content
#' 
#' Internally, this is a wrapper for \code{\link[tm.plugin.koRpus:simpleCorpus]{simpleCorpus}} you can use
#' to analyze texts from more than one source.
#'
#' @param path Usually a charcter vector with the path to the texts to analyse. Below this path, texts
#'    must be ordered into subfolders named exactly like defined by \code{sources}. However, if \code{format="obj"},
#'    you use this argument to provide the texts themselves as a list of named character vectors, where the name
#'    must match the names of \code{sources} and each element of the vectors is a single text.
#' @param sources A named character vector defining all sources to regard. The names of each entry will be used
#'    internally for the sources.
#' @param topic A character string naming the topic covered.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the text in a given object, like
#'    a character vector. See \code{\link[tm.plugin.koRpus:simpleCorpus]{simpleCorpus}} for more information.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#'    This value is passed through to simpleCorpus.
#' @param ... Additional options, passed through to \code{simpleCorpus}.
#' @import tm
#' @export
#' @examples
#' \dontrun{
#' myTopic <- sourcesCorpus("~/data/foo", sources=c(bar="The Bar Magazine", baz="BAZ Monthly"))
#' 
#' # providing text directly
#' myTopic <- sourcesCorpus(
#'   list(
#'     source1=c("the first text.", "the second text."),
#'     source2=c("the third text.", "the last text.")
#'   ),
#'   sources=c(
#'     source1="The Important",
#'     source2="Must Read Magazine"
#'   ),
#'   topic="short stories",
#'   format="obj"
#' )
#' }

sourcesCorpus <- function(
                  path,
                  sources,
                  topic="",
                  format="file",
                  mc.cores=getOption("mc.cores", 1L),
                  ...
                ){

    all.texts <- new("kRp.sourcesCorpus")

    if(identical(format, "file")){
      for (src in names(sources)){
        slot(all.texts, "paths")[[src]] <- list.files(file.path(path, sources[src]), full.names=TRUE)
        slot(all.texts, "files")[[src]] <- list.files(file.path(path, sources[src]))
        numTexts <- length(slot(all.texts, "paths")[[src]])

        topicText <- ifelse(identical(topic, ""), "", paste0("topic \"", topic, "\", "))
        texts_desc <- ifelse(numTexts > 1, " texts...", " text...")
        message(paste0("processing ", topicText, "source \"", sources[src], "\", ", numTexts, texts_desc))
        
        slot(all.texts, "summary")[[src]] <- list()

        slot(all.texts, "sources")[[src]] <- simpleCorpus(dir=file.path(path,sources[src]), source=src, topic=topic, mc.cores=mc.cores, ...)
      }
    } else if(identical(format, "obj")){
      for (src in names(sources)){
        numTexts <- length(path[[src]])
        slot(all.texts, "paths")[[src]] <- slot(all.texts, "files")[[src]] <- rep("", numTexts)

        topicText <- ifelse(identical(topic, ""), "", paste0("topic \"", topic, "\", "))
        texts_desc <- ifelse(numTexts > 1, " texts...", " text...")
        message(paste0("processing ", topicText, "source \"", sources[src], "\", ", numTexts, texts_desc))
        
        slot(all.texts, "summary")[[src]] <- list()

        slot(all.texts, "sources")[[src]] <- simpleCorpus(dir=path[[src]], source=src, topic=topic, format=format, mc.cores=mc.cores, ...)
      }
    } else {
      stop(simpleError(paste0("invalid value for \"format\":\n  \"", format, "\"")))
    }

    return(all.texts)
}
