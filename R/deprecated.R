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

#' @export
simpleCorpus <- function(
                dir=".",
                lang="kRp.env",
                tagger="kRp.env",
                encoding="",
                pattern=NULL,
                recursive=FALSE,
                ignore.case=FALSE,
                mode="text",
                source="",
                topic="",
                format="file",
                mc.cores=getOption("mc.cores", 1L),
                ...
){
  .Deprecated(new="readCorpus")
  return(readCorpus(
    dir=dir,
    hierarchy=list(),
    lang=lang,
    tagger=tagger,
    encoding=encoding,
    pattern=pattern,
    recursive=recursive,
    ignore.case=ignore.case,
    mode=mode,
    format="file",
    mc.cores=mc.cores,
    category="corpus",
    id="",
    level=0,
    ...
  ))
}

#' @export
sourcesCorpus <- function(
                  path,
                  sources,
                  topic="",
                  format="file",
                  mc.cores=getOption("mc.cores", 1L),
                  ...
){
  .Deprecated(new="readCorpus", old="sourcesCorpus")
  hierarchy <- names(sources)
  names(hierarchy) <- sources
  return(readCorpus(
    dir=path,
    hierarchy=list(source=hierarchy),
    format=format,
    mc.cores=mc.cores,
    ...
  ))
}

#' @export
topicCorpus <- function(
                paths,
                sources,
                format="file",
                mc.cores=getOption("mc.cores", 1L),
                ...
){
  .Deprecated(new="readCorpus")
  hierarchy <- names(sources)
  names(hierarchy) <- sources
  return(readCorpus(
    dir=dirname(paths[[1]]),
    hierarchy=list(
      topic=sapply(paths, basename),
      source=hierarchy
    ),
    format=format,
    mc.cores=mc.cores,
    ...
  ))
}
