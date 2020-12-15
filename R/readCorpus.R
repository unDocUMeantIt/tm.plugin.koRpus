# Copyright 2018-2020 Meik Michalke <meik.michalke@hhu.de>
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

#' Create kRp.corpus objects from text files or data frames
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
#'    exactly as defined by \code{hierarchy}. If \code{hierarchy} is an empty list, all text files located in
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
#' @param id A character string describing the main subject/purpose of the text corpus.
#' @param ... Additional options which are passed through to the defined \code{tagger}.
#' @return An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @importFrom tm VCorpus DirSource VectorSource DataframeSource
#' @importFrom NLP meta<-
#' @importFrom parallel mclapply
#' @importFrom koRpus get.kRp.env
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @export
#' @example inst/examples/if_lang_en_clause_start.R
#' @examples
#'   # "flat" corpus, parse all texts in the given dir
#'   myCorpus <- readCorpus(
#'     dir=file.path(
#'       path.package("tm.plugin.koRpus"), "examples", "corpus", "Winner", "Wikipedia_prev"
#'     ),
#'     # use tokenize() so examples run without a TreeTagger installation
#'     tagger="tokenize",
#'     lang="en"
#'   )
#'  
#'   # corpus with one category names "Source"
#'   myCorpus <- readCorpus(
#'     dir=file.path(
#'       path.package("tm.plugin.koRpus"), "examples", "corpus", "Winner"
#'     ),
#'     hierarchy=list(
#'       Source=c(
#'         Wikipedia_prev="Wikipedia (old)",
#'         Wikipedia_new="Wikipedia (new)"
#'       )
#'     ),
#'     tagger="tokenize",
#'     lang="en"
#'   )
#'  
#'   # two hieraryhical levels, "Topic" and "Source"
#'   myCorpus <- readCorpus(
#'     dir=file.path(path.package("tm.plugin.koRpus"), "examples", "corpus"),
#'     hierarchy=list(
#'       Topic=c(
#'         Winner="Reality Winner",
#'         Edwards="Natalie Edwards"
#'       ),
#'       Source=c(
#'         Wikipedia_prev="Wikipedia (old)",
#'         Wikipedia_new="Wikipedia (new)"
#'       )
#'     ),
#'     tagger="tokenize",
#'     lang="en"
#'   )
#'  
#'   # get hierarchy from directory tree
#'   myCorpus <- readCorpus(
#'     dir=file.path(path.package("tm.plugin.koRpus"), "examples", "corpus"),
#'     hierarchy=TRUE,
#'     tagger="tokenize",
#'     lang="en"
#'   )
#'   
#'   \dontrun{
#'     # if the same corpus is available as TIF compliant data frame
#'     myCorpus <- readCorpus(
#'       dir=myCorpus_df,
#'       hierarchy=list(
#'         Topic=c(
#'           Winner="Reality Winner",
#'           Edwards="Natalie Edwards"
#'         ),
#'         Source=c(
#'           Wikipedia_prev="Wikipedia (old)",
#'           Wikipedia_new="Wikipedia (new)"
#'         )
#'       ),
#'       lang="en",
#'       format="obj"
#'     )
#'   }
#' @example inst/examples/if_lang_en_clause_end.R
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
  id="",
  ...
){
  # try to get the hierarchy directly from te directory tree
  if(isTRUE(hierarchy)){
    hierarchy <- hierarchy_from_dirtree(dir)
  } else {}

  do_files <- do_object <- FALSE
  if(identical(format, "file")){
    full_hier_info <- corpus_files(
      dir=dir,
      hierarchy=hierarchy,
      fsep=.Platform$file.sep,
      full_list=TRUE
    )
    full_hier_paths <- normalizePath(file.path(dir, full_hier_info[["hier_paths"]]), mustWork=TRUE)
    do_files <- TRUE
  } else if(identical(format, "obj")){
    if(all(!is.data.frame(dir), !is.character(dir))){
      stop(simpleError("\"dir\" must be a character vecor or data frame!"))
    } else {}
    full_hier_info <- corpus_files(
      dir=dir,
      hierarchy=hierarchy,
      full_list=TRUE
    )
    do_object <- TRUE
  } else {
    stop(simpleError(paste0("invalid value for \"format\":\n  \"", format, "\"")))
  }
  all_files <- full_hier_info[["all_files"]]

  if(identical(lang, "kRp.env")){
    lang <- get.kRp.env(lang=TRUE)
  } else {}

  result <- kRp.corpus(
    lang=lang
  )
  if(all(is.list(hierarchy), length(hierarchy) > 0)){
    corpusHierarchy(result) <- hierarchy
  } else {}

  message("Processing corpus...")
  if(do_files){
    corpusTm(result) <- VCorpus(
      DirSource(
        full_hier_paths,
        encoding=encoding,
        pattern=pattern,
        recursive=recursive,
        ignore.case=ignore.case,
        mode=mode
      ),
      readerControl=list(language=lang)
    )
  } else if(do_object){
    if(is.data.frame(dir)){
      corpus_source <- DataframeSource(dir)
    } else {
      corpus_source <- VectorSource(dir)
    }
    corpusTm(result) <- VCorpus(
      corpus_source,
      readerControl=list(language=lang)
    )
  } else {}

  meta(corpusTm(result), tag="id", type="local") <- all_files[["doc_id"]]
  for (thisCol in colnames(all_files)) {
    meta(corpusTm(result), tag=thisCol, type="indexed") <- meta(corpusTm(result), tag=thisCol, type="local") <- as.character(all_files[[thisCol]])
  }

  init_df <- init_corpus_tokens(hierarchy=hierarchy)
  hier_cols <- names(full_hier_info[["hier_names"]])
  corpusTagged <- mclapply(
    seq_along(corpusTm(result)),
    function(thisTextNum){
      thisText <- corpusTm(result)[[thisTextNum]]
      import_status(
        doc_id=all_files[thisTextNum,"doc_id"],
        all_files=all_files,
        hier_cols=hier_cols
      )
      tagged <- taggerFunction(
        text=thisText[["content"]],
        lang=lang,
        tagger=tagger,
        doc_id=all_files[thisTextNum,"doc_id"],
        ...
      )
      if(!identical(names(init_df), names(taggedText(tagged)))){
        tagged_df <- taggedText(tagged)
        missingCols <- names(init_df)[!names(init_df) %in% names(tagged_df)]
        for (thisCat in missingCols) {
          tagged_df[[thisCat]] <- factor(
            all_files[all_files[["doc_id"]] == all_files[thisTextNum,"doc_id"], thisCat],
            levels=levels(init_df[[thisCat]])
          )
        }
        taggedText(tagged) <- tagged_df
      } else {}
      return(tagged)
    },
    mc.cores=mc.cores
  )
  names(corpusTagged) <- all_files[["doc_id"]]
  stopwords_found <- unlist(mclapply(
    corpusTagged,
    function(thisTaggedText){
      sum(thisTaggedText[["stop"]])
    },
    mc.cores=mc.cores
  ))
  stopwords_list <- list(...)[["stopwords"]]
  if(any(
    length(stopwords_list) > 0,
    sum(stopwords_found) > 0,
    na.rm=TRUE
  )){
    corpusStopwords(result) <- list(
      stopwords=stopwords_list,
      sum=stopwords_found
    )
  } else {}

  corpusTagged_df <- do.call(rbind, mclapply(corpusTagged, taggedText, mc.cores=mc.cores))
  row.names(corpusTagged_df) <- NULL

  taggedText(result) <- corpusTagged_df

  describe(result) <- mclapply(corpusTagged, describe, mc.cores=mc.cores)

  return(result)
}


## function taggerFunction()
#' @importFrom koRpus tokenize treetag
taggerFunction <- function(text, lang, tagger=tagger, doc_id=NA, ...) {
  if(identical(tagger, "tokenize")){
    return(tokenize(txt=text, format="obj", lang=lang, doc_id=doc_id, ...))
  } else {
    return(treetag(file=text, treetagger=tagger, format="obj", lang=lang, doc_id=doc_id, ...))
  }
} ## end function taggerFunction()


## function file_path_from_dir()
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
} ## end function file_path_from_dir()


## function import_status()
# as an easy way to get some status feedback, check if the current doc_id
# is the first one of a given (sub)category, and if so, write a message
import_status <- function(doc_id, all_files, hier_cols){
  file_num <- match(doc_id, all_files[["doc_id"]])
  for (hier_level in seq_len(length(hier_cols))){
    if (hier_level > 1){
      # subset the all_files data frame to get the mathes we're interested in
      for (this_cat_num in 1:(hier_level - 1)){
        this_cat <- hier_cols[this_cat_num]
        this_cat_name <- all_files[file_num, this_cat]
        all_files <- all_files[all_files[[this_cat]] == this_cat_name,]
      }
      file_num <- match(doc_id, all_files[["doc_id"]])
    } else {}
    cat_name <- all_files[file_num, hier_cols[hier_level]]
    cat_start <- match(cat_name, all_files[[hier_cols[hier_level]]])
    if(identical(cat_start, file_num)){
      num_texts <- sum(all_files[[hier_cols[hier_level]]] == cat_name)
      msgText <- paste0(
        paste0(rep("  ", hier_level), collapse=""),
        paste0(hier_cols[hier_level], " \"", cat_name, "\", ", num_texts),
        ifelse(num_texts > 1, " texts...", " text...")
      )
      message(msgText)
    } else {}
  }
} ## end function import_status()
