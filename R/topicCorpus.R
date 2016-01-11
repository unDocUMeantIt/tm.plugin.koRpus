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

#' Function to create kRp.topicCorpus objects from directory or object content
#' 
#' Internally, this is a wrapper for \code{\link[tm.plugin.koRpus:sourcesCorpus]{sourcesCorpus}} you can use
#' to analyze texts both from more than one source, and on different topics.
#' 
#' @param paths A named list, usually with the paths to the texts to analyze. Each named list element
#'    will define one topic. Below this path, texts must be ordered into subfolders named exactly like defined
#'    by \code{sources}. However, if \code{format="obj"}, you use this argument to provide the texts themselves
#'    as a list of named lists of named character vectors, where the names must match the names of \code{sources}
#'    and each element of the vectors is a single text. See the examples below.
#' @param sources A named character vector defining all sources to regard. The names of each entry will be used
#'    internally for the sources.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the text in a given object, like
#'    a character vector. See \code{\link[tm.plugin.koRpus:sourcesCorpus]{sourcesCorpus}} for more information.
#' @param ... Additional options, passed through to \code{sourcesCorpus}.
#' @export
#' @examples
#' \dontrun{
#' myOrderedCorpus <- topicCorpus(
#'   paths=list(
#'     lottery="~/data/foo/lottery",
#'     waste="~/data/foo/waste"),
#'   sources=c(
#'     bar="The Bar Magazine",
#'     baz="BAZ Monthly")
#' )
#' 
#' # providing texts directly
#' myOrderedCorpus <- topicCorpus(
#'   paths=list(
#'     lottery=list(
#'       source1=c("the first text.", "the second text."),
#'       source2=c("the third text.", "the fourth text.")
#'     ),
#'     waste=list(
#'       source1=c("the fifth text.", "the sixth text."),
#'       source2=c("the seventh text.", "the last text.")
#'     )
#'   ),
#'   sources=c(
#'     source1="The Important",
#'     source2="Must Read Magazine"
#'   ),
#'   format="obj"
#' )
#' }

topicCorpus <- function(paths, sources, format="file", ...){
  all.texts <- new("kRp.topicCorpus")

  for (topic in names(paths)) {
    slot(all.texts, "topics")[[topic]] <- sourcesCorpus(path=paths[[topic]], sources=sources, topic=topic, format=format, ...)
  }
  
  return(all.texts)
}
