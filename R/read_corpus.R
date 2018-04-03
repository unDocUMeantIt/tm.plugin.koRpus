# Copyright 2018 Meik Michalke <meik.michalke@hhu.de>
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

#' Convenience wrapper for simpleCorpus, sourcesCorpus and topicCorpus
#'
#' This wrapper function simplifies the use of the three main corpus parsers in this package,
#' \code{\link[tm.plugin.koRpus:simpleCorpus]{simpleCorpus}},
#' \code{\link[tm.plugin.koRpus:sourcesCorpus]{sourcesCorpus}}, and
#' \code{\link[tm.plugin.koRpus:topicCorpus]{topicCorpus}}.
#' Depending on the amount and structure of the data given, it will invoke one of those
#' main functions.
#' 
#' The wrapper explicitly doesn't support all features of the single functions. The main
#' restriction is that the \code{type} argument is not available, instead  \code{type="file"} is
#' always assumed. That is, it is not possible to supply the corpus content directly as
#' character vectors.
#' 
#' When the parameter docs explain that element names or values must match certain subdirectories, it
#' means that directories of those names must be present. The directory can have additional subdirectories
#' or files, they will simply be ignored.
#'
#' @param dir A charcter string with the path to the root directory of the text corpus. Below this path, texts
#'    must be ordered into subfolders named exactly like defined by \code{topics} and \code{sources}. If both
#'    \code{topics} and \code{sources} have exactly one value, \code{simpleCorpus} will be called to parse all text files
#'    below \code{dir}.
#' @param sources An optional named character vector defining all sources to regard. If more than one value is provided,
#'    the names of each entry must match the names of subdirectories below \code{dir}, depending on the value of \code{topics}:
#'    If \code{topics} has only one value, names of \code{sources} must directly match subdirectories of \code{dir} and \code{sourcesCorpus}
#'    will be called; if \code{topics} has more values, names must match subdirectories of \code{dir/<topic>}
#'    and \code{topicCorpus} will be called instead. If only one value is provided, its name will be ignored and can be omitted.
#' @param topics An optional charcter vector naming the different topics the corpus is divided into. If more than one value is provided,
#'    they must match the subfolders of \code{dir}, and you must also provide at least one valid value for \code{sources}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... Additional options, passed through to \code{simpleCorpus}, \code{sourcesCorpus}, or \code{topicCorpus}
#'    respectively.
#' @return Either an object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}},
#'    \code{\link[tm.plugin.koRpus:kRp.sourcesCorpus-class]{kRp.sourcesCorpus}}, or
#'    \code{\link[tm.plugin.koRpus:kRp.topicCorpus-class]{kRp.topicCorpus}}, depending on the input.
#' @export
#' @examples
#' \dontrun{
#' # invoking simpleCorpus()
#' myCorpus <- read_corpus(
#'   dir="~/data/foo",
#'   source="The Bar Magazine",
#'   topic="short stories"
#' )
#'
#' # invoking sourcesCorpus()
#' mySourceCorpus <- read_corpus(
#'   dir="~/data/foo",
#'   sources=c(
#'     bar="The Bar Magazine",
#'     baz="BAZ Monthly"
#'   )
#' )
#'
#' # invoking topicCorpus()
#' myTopicCorpus <- read_corpus(
#'   dir="~/data/foo",
#'   sources=c(
#'     bar="The Bar Magazine",
#'     baz="BAZ Monthly"
#'   ),
#'   topics=c("lottery","waste")
#' )
#' }
read_corpus <- function(
                dir,
                sources="",
                topics="",
                mc.cores=getOption("mc.cores", 1L),
                ...
               ){
  if(all(length(sources) == 1, length(topics) == 1)){
    results <- simpleCorpus(
      dir=dir,
      source=sources,
      topic=topics,
      format="file",
      mc.cores=mc.cores,
      ...
    )
  } else if(all(length(sources) > 1, length(topics) == 1)){
    results <- sourcesCorpus(
      path=dir,
      sources=sources,
      topic=topics,
      format="file",
      mc.cores=mc.cores,
      ...
    )
  } else if(all(length(sources) > 1, length(topics) > 1)){
    topic_paths <- file.path(dir, topics)
    names(topic_paths) <- topics
    results <- topicCorpus(
      paths=topic_paths,
      sources=sources,
      format="file",
      mc.cores=mc.cores,
      ...
    )
  } else {
      stop(simpleError("You must at least provide one valid value for both 'sources' and 'topics'!"))
  }
  return(results)
}
