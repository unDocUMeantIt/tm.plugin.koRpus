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

#' Function to create kRp.hierarchy objects from hierachically structured directories
#' 
#' @section Hierarchy:
#' To import a hierarchically structured text corpus you must categorize all texts in a directory
#' structure that resembles the hierarchy. If for example you would like to import a corpus on two
#' different topics and two differnt sources, your hierarchy has two nested levels (topic and source).
#' The root directory \code{dir} would then need to have two subdirectories (one for each topic)
#' which in turn must have two subdirectories (one for each source), and the actual text files
#' are found in those.
#' 
#' To use this hierarchical structure in \code{readCorpus}, the \code{hierarchy} argument is used.
#' It is a named list, where each list item represents one hierachical level (here again topic and source),
#' and its value is a named character vector describing the actual topics and sources to be used. It is
#' important to understand how these character vectors are treated: The names of elements must exactly match
#' the corresponding subdirectroy name, whereas the value is a free text description. The names of the
#' list items however describe the hierachical level and are not matched with directory names.
#' 
#' @param dir File path to the root directory of the text corpus. Below this path, texts
#'    can be ordered into subfolders named exactly like defined by \code{hierarchy}. If \code{hierarchy}
#'    is an emtpy list, all text files located in \code{dir} are parsed without a hierachical structure.
#' @param hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#'    See section Hierarchy for details.
#' @param lang A character string naming the language of the analyzed corpus.
#'    See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'    If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#'    This information will also be passed to the \code{readerControl} list of the \code{VCorpus} call.
#' @param tagger A character string pointing to the tokenizer/tagger command you want to use for basic text analysis.
#'    Defaults to \code{tagger="kRp.env"} to get the settings by \code{\link[koRpus:get.kRp.env]{get.kRp.env}}.
#'    Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
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
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the text in a given object,
#'    like a character vector. If the latter and \code{\link[koRpus:treetag]{treetag}} is used as the \code{tagger},
#'    texts will be written to temporary files for the process (see \code{dir}).
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#'    This value is passed through to simpleCorpus.
#' @param category A character string describing the root level of the corpus.
#' @param id A character string describing the main subject/purpose of the text corpus.
#' @param level A numeric value defining the hierachical level, where \code{level=0} is the actual collection of texts,
#'    and higher values indicate a hierachical category. You should not manipulate this value yourself, it is used
#' @param ... Additional options which are passed through to the defined \code{tagger}.
#' @value An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @export
#' @examples
#' \dontrun{
#' myCorpus <- readCorpus(
#'   dir=file.path(path.package("tm.plugin.koRpus"), "tests", "testthat", "samples"),
#'   hierarchy=list(
#'     Topic=c(
#'       C3S="C3S",
#'       GEMA="GEMA"
#'     ),
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   )
#' )
#' }

readCorpus <- function(
                dir,
                hierarchy=list(),
                lang="kRp.env",
                tagger="kRp.env",
                encoding="",
                pattern=NULL,
                recursive=FALSE,
                ignore.case=FALSE,
                mode="text",
                format="file",
                mc.cores=getOption("mc.cores", 1L),
                category="corpus",
                id="",
                level=0,
                ...
               ){
  # analysis is done recursively by an internal function
  result <- readCorpus_internal(
    dir=dir,
    hierarchy=hierarchy,
    lang=lang,
    tagger=tagger,
    encoding=encoding,
    pattern=pattern,
    recursive=recursive,
    ignore.case=ignore.case,
    mode=mode,
    format=format,
    mc.cores=mc.cores,
    level=length(hierarchy),
    category=category,
    id=id,
    ...
  )
  return(result)
}

readCorpus_internal <- function(
                        dir,
                        hierarchy,
                        lang,
                        tagger,
                        encoding,
                        pattern,
                        recursive,
                        ignore.case,
                        mode,
                        format,
                        mc.cores,
                        level=0,
                        id,
                        category,
                        ...
                      ){
  taggerFunction <- function(text, lang, tagger=tagger, doc_id=NA, ...) {
    if(identical(tagger, "tokenize")){
      return(tokenize(txt=text, format="obj", lang=lang, doc_id=doc_id, ...))
    } else {
      return(treetag(file=text, treetagger=tagger, format="obj", lang=lang, doc_id=doc_id, ...))
    }
  }
  
  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  if(length(hierarchy) > 0){
    # start recursion on top level of hierarchy
    thisLevel <- hierarchy[[1]]
    subdirs <- normalizePath(file.path(dir, names(thisLevel)), mustWork=TRUE)
    children <- lapply(
      seq_along(subdirs),
      function(thisSubdirNum){
        thisSubdir <- subdirs[thisSubdirNum]
        readCorpus_internal(
          dir=thisSubdir,
          hierarchy=hierarchy[-1],
          lang=lang,
          tagger=tagger,
          encoding=encoding,
          pattern=pattern,
          recursive=recursive,
          ignore.case=ignore.case,
          mode=mode,
          format=format,
          mc.cores=mc.cores,
          level=level - 1,
          category=names(hierarchy)[1],
          id=names(thisLevel)[thisSubdirNum],
          ...
        )
      }
    )
    result <- kRp_hierarchy(
      level=level,
      category=category,
      id=id,
      path=dir,
      children=children
    )
  } else {
    # actually parse texts
    result <- kRp_hierarchy(
      level=level,
      category=category,
      id=id,
      path=dir,
      files=as.list(list.files(dir))
    )
    if(identical(format, "file")){
      slot(result, "raw") <- list(VCorpus(
        DirSource(
          dir,
          encoding=encoding,
          pattern=pattern,
          recursive=recursive,
          ignore.case=ignore.case,
          mode=mode
        ),
        readerControl=list(language=lang)
      ))
    } else if(identical(format, "obj")){
      slot(result, "raw") <- list(VCorpus(
        VectorSource(
          dir
        ),
        readerControl=list(language=lang)
      ))
    } else {
      stop(simpleError(paste0("invalid value for \"format\":\n  \"", format, "\"")))
    }

    names(slot(result, "raw")) <- "tm"
    numTexts <- length(corpusTm(result))
    nameNum <- sprintf("%02d", 1:numTexts)
    text_ID <- textID(src=id, topic=category, nameNum=nameNum)
    meta(corpusTm(result), tag="textID") <- text_ID
    corpusMeta(result, "category") <- category
    corpusMeta(result, "id") <- id

    corpusTagged <- mclapply(
      1:numTexts,
      function(thisTextNum){
        thisText <- corpusTm(result)[[thisTextNum]]
        taggerFunction(text=thisText[["content"]], lang=lang, tagger=tagger, doc_id=text_ID[thisTextNum], ...)
      },
      mc.cores=mc.cores
    )
    names(corpusTagged) <- text_ID
    corpusMeta(result, "stopwords") <- unlist(mclapply(
      corpusTagged,
      function(thisTaggedText){
        sum(thisTaggedText[["stop"]])
      },
      mc.cores=mc.cores
    ))
    slot(result, "tagged") <- corpusTagged
  }
  return(result)
}
