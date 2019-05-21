# Copyright 2018-2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Create kRp.hierarchy objects from text files or data frames
#' 
#' You can either read a corpus from text files (one file per text, also see the Hierarchy section below)
#' or from TIF compliant data frames (see the Data frames section below).
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
#' @section Data frames:
#' In order to import a corpus from a data frame, the object must be in Text Interchange Format (TIF)
#' as described by [1]. As a minimum, the data frame must have two character columns, \code{doc_id}
#' and \code{text}.
#' 
#' You can provide additional information on hierarchical categories by using further
#' columns, where the column name must match the category name (hierachical level). The order of those
#' columns in the data frame is not important, as you must still fully define the hierarchical structure
#' using the \code{hierarchy} argument. All columns you omit are ignored, but the values used in
#' the \code{hierarchy} list and the respective columns must match, as rows with unmatched category levels
#' will also be ignored.
#' 
#' Note that the special column names \code{path} and \code{file} will also be imported automatically.
#' 
#' @param dir Either a file path to the root directory of the text corpus, or a TIF compliant data frame.
#'    If a directory path (character string), texts can be recursively ordered into subfolders named
#'    exactly as defined by \code{hierarchy}. If \code{hierarchy} is an emtpy list, all text files located in
#'    \code{dir} are parsed without a hierachical structure. If a data frame, also set \code{format="obj"}
#'    and provide hierarchy levels as additional columns, as described in the Data frames section.
#' @param hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#'    If \code{TRUE} instead, the hierarchy structure is taken directly from the directory tree.
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
#' @param ... Additional options which are passed through to the defined \code{tagger}.
#' @return An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @importFrom tm VCorpus DirSource VectorSource DataframeSource
#' @importFrom NLP meta<-
#' @importFrom parallel mclapply
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @export
#' @examples
#' \dontrun{
#' # "flat" corpus, parse all texts in the given dir
#' myCorpus <- readCorpus(
#'   dir=file.path(
#'     path.package("tm.plugin.koRpus"), "tests", "testthat", "samples", "C3S", "Wikipedia_alt"
#'   )
#' )
#'
#' # corpus with one category names "Source"
#' myCorpus <- readCorpus(
#'   dir=file.path(
#'     path.package("tm.plugin.koRpus"), "tests", "testthat", "samples", "C3S"
#'   ),
#'   hierarchy=list(
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   )
#' )
#'
#' # two hieraryhical levels, "Topic" and "Source"
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
#' 
#' # if the same corpus is available as TIF compliant data frame
#' myCorpus <- readCorpus(
#'   dir=myCorpus_df,
#'   hierarchy=list(
#'     Topic=c(
#'       C3S="C3S",
#'       GEMA="GEMA"
#'     ),
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   ),
#'   format="obj"
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
                ...
               ){
  # try to get the hierarchy directly from te directory tree
  if(isTRUE(hierarchy)){
    hierarchy <- hierarchy_from_dirtree(dir)
  } else {}

  # analysis is done recursively by an internal function
  hierarchy_branch <- matrix(c(id, names(id)), nrow=2, dimnames=list(c("id","dir"), category))

  result <- readCorpus_internal(
    dir=dir,
    hierarchy=hierarchy,
    all_hierarchy=hierarchy,
    hierarchy_branch=hierarchy_branch,
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
    all_levels=length(hierarchy),
    category=category,
    id=id,
    text_id=id,
    ...
  )
  return(result)
}

readCorpus_internal <- function(
                        dir,
                        hierarchy,
                        all_hierarchy,
                        hierarchy_branch,
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
                        all_levels=0,
                        category,
                        id,             # ID of current level
                        text_id,        # will grow longer with each hierarchy level
                        ...
                      ){
  do_files <- do_object <- FALSE
  if(identical(format, "file")){
    do_files <- TRUE
  } else if(identical(format, "obj")){
    do_object <- TRUE
  } else {
    stop(simpleError(paste0("invalid value for \"format\":\n  \"", format, "\"")))
  }

  taggerFunction <- function(text, lang, tagger=tagger, doc_id=NA, ...) {
    if(identical(tagger, "tokenize")){
      return(tokenize(txt=text, format="obj", lang=lang, doc_id=doc_id, ...))
    } else {
      return(treetag(file=text, treetagger=tagger, format="obj", lang=lang, doc_id=doc_id, ...))
    }
  }

  file_path_from_dir <- function(d){
    result <- list(
      file_names=list(),
      path_name=c()
    )
    if(is.data.frame(d)){
      if("file" %in% colnames(d)){
        result[["file_names"]] <- list(as.character(d[["file"]]))
      } else {}
      if("path" %in% colnames(d)){
        result[["path_name"]] <- as.character(d[["path"]])
      } else {}
    } else {}
    return(result)
  }

  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  if(length(hierarchy) > 0){
    # start recursion on top level of hierarchy
    thisLevel <- hierarchy[[1]]
    if(do_files){
      subdirs <- normalizePath(file.path(dir, names(thisLevel)), mustWork=TRUE)
    } else if(do_object){
      if(names(hierarchy)[1] %in% colnames(dir)){
        subdirs <- unique(as.character(dir[[names(hierarchy)[1]]]))
      } else {
        stop(simpleError(paste0("Invalid hierarchy category (not in data frame): ", category)))
      }
    }
    msgText <- paste0(
      paste0(rep("  ", all_levels - level), collapse=""),
      ifelse(identical(all_levels, level), "processing ", ""),
      ifelse(nchar(category) > 0, category, ""),
      ifelse(nchar(id) > 0, paste0(" \"", id, "\""), "")
    )
    message(msgText)
    children <- lapply(
      seq_along(subdirs),
      function(thisSubdirNum){
        thisSubdir <- subdirs[thisSubdirNum]
        thisID <- thisLevel[thisSubdirNum]
        thisIDName <- names(thisLevel)[thisSubdirNum]
        thisCategory <- names(hierarchy)[1]
        new_hierarchy_branch <- matrix(c(thisID, names(thisID)), nrow=2, dimnames=list(c("id","dir"), thisCategory))
        if(do_files){
          do_subdir <- thisSubdir
        } else if(do_object){
          do_subdir <- dir[dir[[thisCategory]] %in% thisSubdir,]
        } else {}
        readCorpus_internal(
          dir=do_subdir,
          hierarchy=hierarchy[-1],
          all_hierarchy=all_hierarchy,
          hierarchy_branch=cbind(hierarchy_branch, new_hierarchy_branch),
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
          all_levels=all_levels,
          category=thisCategory,
          id=thisID,
          text_id=paste0(text_id, thisIDName),
          ...
        )
      }
    )
    names(children) <- sapply(children, corpusID)
    if(do_files){
      path_name <- dir
    } else if(do_object){
      fp_lookup <- file_path_from_dir(d=dir)
      path_name <- fp_lookup[["path_name"]]
    } else {}
    result <- kRp_hierarchy(
      level=as.integer(level),
      category=category,
      id=id,
      path=path_name,
      children=children,
      meta=list(
        hierarchy=all_hierarchy,
        hierarchy_branch=hierarchy_branch,
        category=category,
        id=id
      )
    )
  } else {
    if(do_files){
      file_names <- as.list(list.files(dir))
      path_name <- dir
    } else if(do_object){
      fp_lookup <- file_path_from_dir(d=dir)
      file_names <- fp_lookup[["file_names"]]
      path_name <- fp_lookup[["path_name"]]
    } else {}
    # actually parse texts
    result <- kRp_hierarchy(
      level=as.integer(level),
      category=category,
      id=id,
      path=path_name,
      files=file_names,
      meta=list(
        hierarchy=all_hierarchy,
        hierarchy_branch=hierarchy_branch,
        category=category,
        id=id
      )
    )
    numTexts <- length(corpusFiles(result))
    msgText <- paste0(
      paste0(rep("  ", all_levels - level), collapse=""),
      ifelse(identical(all_levels, level), "processing ", ""),
      ifelse(nchar(category) > 0, category, ""),
      ifelse(nchar(id) > 0, paste0(" \"", id, "\", "), ", "),
      numTexts,
      ifelse(numTexts > 1, " texts...", " text...")
    )
    message(msgText)
    if(do_files){
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
    } else if(do_object){
      if(is.data.frame(dir)){
        corpus_source <- DataframeSource(dir)
      } else {
        corpus_source <- VectorSource(dir)
      }
      slot(result, "raw") <- list(VCorpus(
        corpus_source,
        readerControl=list(language=lang)
      ))
    } else {}

    names(slot(result, "raw")) <- "tm"
    numTexts <- length(corpusTm(result))
    nameNum <- sprintf("%02d", 1:numTexts)
    text_id <- paste0(text_id, nameNum)
    meta(corpusTm(result), tag="textID") <- text_id
    # add directory and filename
    meta(corpusTm(result), tag="path") <- path_name
    meta(corpusTm(result), tag="file") <- unlist(file_names)
    # add all available hierarchy info
    for(this_branch in colnames(hierarchy_branch)){
      meta(corpusTm(result), tag=this_branch) <- hierarchy_branch["id",this_branch]
    }

    corpusTagged <- mclapply(
      1:numTexts,
      function(thisTextNum){
        thisText <- corpusTm(result)[[thisTextNum]]
        taggerFunction(text=thisText[["content"]], lang=lang, tagger=tagger, doc_id=text_id[thisTextNum], ...)
      },
      mc.cores=mc.cores
    )
    names(corpusTagged) <- text_id
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
