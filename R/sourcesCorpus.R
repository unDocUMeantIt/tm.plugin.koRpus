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

#' Function to create kRp.sourcesCorpus objects from directory content
#' 
#' @param path A charcter vector with the path to the texts to analyse. Below this path, texts
#'    must be ordered into subfolders named exactly like defined by \code{sources}.
#' @param sources A named character vector defining all sources to regard. The names of each entry will be used
#'    internally for the sources.
#' @param topic A character string naming the topic covered.
#' @param ... Additional options, passed through to \code{simpleCorpus}.
#' @import tm
#' @export
#' @examples
#' \dontrun{
#' myTopic <- sourcesCorpus("~/data/foo", sources=c(bar="The Bar Magazine", baz="BAZ Monthly"))
#' }

sourcesCorpus <- function(path, sources, topic="", ...){

    all.texts <- new("kRp.sourcesCorpus")

    for (src in names(sources)){
      slot(all.texts, "paths")[[src]] <- list.files(file.path(path, sources[src]), full.names=TRUE)
      slot(all.texts, "files")[[src]] <- list.files(file.path(path, sources[src]))
      numTexts <- length(slot(all.texts, "paths")[[src]])

      topicText <- ifelse(identical(topic, ""), "", paste0("topic \"", topic, "\", "))
      message(paste0("processing ", topicText, "source \"", sources[src], "\", ", numTexts, " texts..."))
      
      slot(all.texts, "summary")[[src]] <- list()

      slot(all.texts, "sources")[[src]] <- simpleCorpus(dir=file.path(path,sources[src]), source=src, topic=topic, ...)
    }

    return(all.texts)
}
