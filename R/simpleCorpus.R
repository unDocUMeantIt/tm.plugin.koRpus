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


#' Function to create kRp.corpus objects from directory content
#' 
#' This function is a combined wrapper that calls \code{\link[tm:DirSource]{DirSource}},
#' \code{\link[tm:VCorpus]{VCorpus}} and \code{\link[koRpus:tokenize]{tokenize}} or
#' \code{\link[koRpus:treetag]{treetag}}.
#' 
#' The result, if succeeded, is a single object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}},
#' which includes all read texts in a \code{tm} style \code{VCorpus} format, as well as in
#' \code{koRpus} style \code{kRp.taggedText} class format.
#' 
#' @param dir Character vector with path names to search for text files, or the actual texts to be analyzed if \code{format="obj"}.
#'    See \code{\link[tm:DirSource]{DirSource}} and \code{\link[tm:VectorSource]{VectorSource}} for details.
#' @param lang A character string naming the language of the analyzed corpus. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'    If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. This information will also be passed to
#'    the \code{readerControl} list of the \code{VCorpus} call.
#' @param tagger A character string pointing to the tokenizer/tagger command you want to use for basic text analysis. Defaults to \code{tagger="kRp.env"} to get the settings by
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param encoding Character string describing the current encoding.
#'    See \code{\link[tm:DirSource]{DirSource}} for details, omitted if \code{format="obj"}.
#' @param pattern A regular expression for file matching.
#'    See \code{\link[tm:DirSource]{DirSource}} for details, omitted if \code{format="obj"}.
#' @param recursive Logical, indicates whether directories should be read recursively.
#'    See \code{\link[tm:DirSource]{DirSource}} for details, omitted if \code{format="obj"}.
#' @param ignore.case Logical, indicates whether \code{pattern} is matched case sensitive.
#'    See \code{\link[tm:DirSource]{DirSource}} for details, omitted if \code{format="obj"}.
#' @param mode Character string defining the reading mode.
#'    See \code{\link[tm:DirSource]{DirSource}} for details, omitted if \code{format="obj"}.
#' @param source Character string, naming the source of the corpus.
#' @param topic Character string, a topic this corpus deals with.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the text in a given object, like
#'    a character vector. If the latter and \code{\link[koRpus:treetag]{treetag}} is used as the \code{tagger},
#'    texts will be written to temporary files for the process (see \code{dir}).
#' @param ... Additional options which are passed through to the defined \code{tagger}.
#' @return An object of class \code{\link[tm.plugin.koRpus]{kRp.corpus-class}}.
#' @import koRpus tm
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
                ...
              ) {

  taggerFunction <- function(text, lang, tagger=tagger, ...) {
    if(identical(tagger, "tokenize")){
      return(tokenize(txt=text, format="obj", lang=lang, ...))
    } else {
      return(treetag(file=text, treetagger=tagger, format="obj", lang=lang, ...))
    }
  }
  
  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  if(identical(format, "file")){
    newCorpus <- new("kRp.corpus",
      raw=list(VCorpus(
        DirSource(
          dir,
          encoding=encoding,
          pattern=pattern,
          recursive=recursive,
          ignore.case=ignore.case,
          mode=mode
        ),
        readerControl=list(language=lang)
      )
    ))
  } else if(identical(format, "obj")){
    newCorpus <- new("kRp.corpus",
      raw=list(VCorpus(
        VectorSource(
          dir
        ),
        readerControl=list(language=lang)
      )
    ))
  } else {
    stop(simpleError(paste0("invalid value for \"format\":\n  \"", format, "\"")))
  }

  names(slot(newCorpus, "raw")) <- "tm"
  numTexts <- length(corpusTm(newCorpus))
  nameNum <- sprintf("%02d", 1:numTexts)
  meta(corpusTm(newCorpus), tag="textID") <- textID(src=source, topic=topic, nameNum=nameNum)
  corpusMeta(newCorpus, "topic") <- topic
  corpusMeta(newCorpus, "source") <- source

  corpusTagged <- lapply(corpusTm(newCorpus), function(thisText){
    taggerFunction(text=thisText[["content"]], lang=lang, tagger=tagger, ...)
  })
  slot(newCorpus, "tagged") <- corpusTagged
  return(newCorpus)
}
