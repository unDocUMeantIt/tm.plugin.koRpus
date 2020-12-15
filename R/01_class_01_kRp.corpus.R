# Copyright 2019-2020 Meik Michalke <meik.michalke@hhu.de>
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


## function init_corpus_tokens()
# initializes the tokens data frame including columns with hierarchy information
init_corpus_tokens <- function(hierarchy=list()){
  kRp_df <- koRpus::taggedText(koRpus::kRp_text())
  if(length(hierarchy) > 0){
    hier_names <- names(hierarchy)
    invalidNames <- hier_names %in% names(kRp_df)
    if(any(invalidNames)){
      stop(simpleError(
        paste0(
          "Invalid hierarchy, names must not match column names in the tokens slot:\n  \"",
          paste0(hier_names[invalidNames], collapse="\", \""),
          "\""
        )
      ))
    } else {}
    initial_df <- data.frame(
      kRp_df,
      matrix(
        ncol=length(hierarchy),
        dimnames=list(c(), hier_names)
      )
    )
    for (thisCat in hier_names) {
      initial_df[[thisCat]] <- factor(NA, levels=hierarchy[[thisCat]])
    }
    return(initial_df)
  } else {
    return(kRp_df)
  }
} ## end function init_corpus_tokens()


#' S4 Class kRp.corpus
#'
#' Objects of this class can contain full text corpora in a hierachical structure. It supports both the \code{tm} package's
#' \code{\link[tm]{Corpus}} class and \code{koRpus}' own object classes and stores them in separated slots.
#' 
#' Objects should be created using the \code{\link[tm.plugin.koRpus:readCorpus]{readCorpus}} function.
#' 
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp.corpus(...)} can be used instead of
#' \code{new("kRp.corpus", ...)}. Whenever possible, stick to
#' \code{\link[tm.plugin.koRpus:readCorpus]{readCorpus}}.
#' 
#' @slot lang A character string, naming the language that is assumed for the tokenized texts in this object.
#' @slot desc A named list of descriptive statistics of the tagged texts.
#' @slot meta A named list. Can be used to store meta information. Currently, no particular format is defined.
#' @slot raw A list of objects of class \code{\link[tm]{Corpus}}.
#' @slot tokens A data frame as used for the \code{tokens} slot in objects of class \code{\link[koRpus:kRp.text-class]{kRp.text}}. In addition to the columns
#'    usually found in those objects, this data frame also has a factor column for each hierarchical category defined (if any).
#' @slot features A named logical vector, indicating which features are available in this object's \code{feat_list} slot.
#'    Common features are listed in the description of the \code{feat_list} slot.
#' @slot feat_list A named list with optional analysis results or other content as used by the defined \code{features}:
#'    \itemize{
#'      \item{\code{hierarchy} }{A named list of named character vectors describing the directory hierarchy level by level.}
#'      \item{\code{hyphen} }{A named list of objects of class \code{\link[sylly:kRp.hyphen-class]{kRp.hyphen}}.}
#'      \item{\code{readability} }{A named list of objects of class \code{\link[koRpus:kRp.readability-class]{kRp.readability}}.}
#'      \item{\code{lex_div} }{A named list of objects of class \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.}
#'      \item{\code{freq} }{The \code{freq.analysis} slot of a \code{\link[koRpus:kRp.txt.freq-class]{kRp.txt.freq}} class object after
#'        \code{\link[tm.plugin.koRpus:freq.analysis]{freq.analysis}} was called.}
#'      \item{\code{corp_freq} }{An object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}, e.g., results of a call to
#'        \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}}.}
#'      \item{\code{diff} }{A named list of \code{diff} features of a \code{\link[koRpus:kRp.text-class]{kRp.text}} object after
#'        a method like \code{\link[tm.plugin.koRpus:textTransform]{textTransform}} was called.}
#'      \item{\code{summary} }{A summary data frame for the full corpus, including descriptive statistics on all texts, as well as
#'        results of analyses like readability and lexical diversity, if available.}
#'      \item{\code{doc_term_matrix} }{A sparse document-term matrix, as produced by \code{\link[tm.plugin.koRpus:docTermMatrix]{docTermMatrix}}.}
#'      \item{\code{stopwords} }{A numeric vector with the total number of stopwords in each text, if stopwords were analyzed during tokenizing or POS tagging.}
#      \item{\code{} }{}
#'    }
#'    See the \code{\link[tm.plugin.koRpus:kRp.corpus_get-methods]{getter and setter methods}} for easy access to these sub-slots.
#'    There can actually be any number of additional features, the above is just a list of those already defined by this package.
#' @note There is also \code{\link[tm.plugin.koRpus:kRp.corpus_get-methods]{getter and setter methods}} for objects of this class.
#' @name kRp.corpus,-class
#' @aliases kRp.corpus,-class kRp.corpus-class
#' @import methods
#' @importFrom tm as.VCorpus
#' @keywords classes
#' @export kRp.corpus
#' @exportClass kRp.corpus
#' @rdname kRp.corpus
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_4_texts.R
#' @example inst/examples/if_lang_en_clause_end.R
#' @examples
#'
#' # manual creation
#' emptyCorpus <- kRp.corpus()
kRp.corpus <- setClass("kRp.corpus",
  representation=representation(
    meta="list",
    raw="list"
  ),
  prototype=prototype(
    lang=character(),
    desc=list(),
    meta=list(),
    raw=list(),
    tokens=init_corpus_tokens(),
    features=logical(),
    feat_list=list()
  ),
  contains="kRp.text"
)

setValidity("kRp.corpus", function(object){
    raw <- slot(object, "raw")
    tokens <- slot(object, "tokens")
    features <- slot(object, "features")
    feat_list <- slot(object, "feat_list")
    hyphen <- feat_list[["hyphen"]]
    readability <- feat_list[["readability"]]
    lex_div <- feat_list[["lex_div"]]
    freq <- feat_list[["freq"]]
    corp_freq <- feat_list[["corp_freq"]]
    obj_summary <- feat_list[["summary"]]
  # TODO: check if hierarchy is resembled by rows in tagged object
    hierarchy <- feat_list[["hierarchy"]]

   missingFeatures <- sapply(
      names(features),
      function(this_feature){
        if(
          all(
            isTRUE(features[[this_feature]]),
            is.null(feat_list[[this_feature]])
          )
        ){
          return(this_feature)
        } else {}
      }
    )
    if(length(missingFeatures) > 0) {
      warning(
        paste0(
          "Invalid object: There's no data for activated \"features\":\n  ",
          paste0(missingFeatures, collapse=", ")
        ),
        call.=FALSE
      )
    } else {}
    tokens.names <- colnames(tokens)
    standard.tokens.names <- colnames(koRpus::taggedText(koRpus::kRp_text()))
    missingCols <- standard.tokens.names[!standard.tokens.names %in% tokens.names]
    if(length(missingCols) > 0){
      warning(
        paste0(
          "Invalid object: Missing columns in slot \"tokens\":\n  ",
          paste0(missingCols, collapse=", ")
        ),
        call.=FALSE)
    } else {}

    classObj <- list(
      "Corpus"=list(name="raw", obj=raw),
      "kRp.hyphen"=list(name="hyphen", obj=hyphen),
      "kRp.readability"=list(name="readability", obj=readability),
      "kRp.TTR"=list(name="lex_div", obj=lex_div),
      "kRp.corp.freq"=list(name="corp_freq", obj=corp_freq),
      "data.frame"=list(name="summary", obj=obj_summary)
    )
    for (thisClassObj in names(classObj)) {
      if(
        all(
          !identical(classObj[[thisClassObj]][["obj"]], list()),
          !is.null(classObj[[thisClassObj]][["obj"]]),
          !all(sapply(classObj[[thisClassObj]][["obj"]], function(x) inherits(x, thisClassObj)))
        )
      ){
        stop(simpleError(paste0("Invalid object: Slot \"", classObj[[thisClassObj]][["name"]], "\" must have entries inheriting from class ", thisClassObj, "!")))
      } else {}
    }

    return(TRUE)
})
