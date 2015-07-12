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

#' Function to create kRp.topicCorpus objects from directory content
#' 
#' @param paths A charcter vector with the path to the texts to analyse. Below this path, texts
#'    must be ordered into subfolders named exactly like defined by \code{sources}.
#' @param sources A named character vector defining all sources to regard. The names of each entry will be used
#'    internally for the sources.
#' @param ... Additional options, passed through to \code{sourcesCorpus}.
#' @export
#' @examples
#' \dontrun{
#' myOrderedCorpus <- topicCorpus(
#'   paths=c(
#'     lottery="~/data/foo/lottery",
#'     waste="~/data/foo/waste"),
#'   sources=c(
#'     bar="The Bar Magazine",
#'     baz="BAZ Monthly")
#' )
#' }

topicCorpus <- function(paths, sources, ...){
  all.texts <- new("kRp.topicCorpus")

  for (topic in names(paths)) {
    slot(all.texts, "topics")[[topic]] <- sourcesCorpus(path=paths[topic], sources=sources, topic=topic, ...)
  }
  
  return(all.texts)
}
