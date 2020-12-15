# Copyright 2015-2020 Meik Michalke <meik.michalke@hhu.de>
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

#' Apply readability() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:readability]{readability}} on all tagged text objects
#' inside the given \code{txt.file} object (using \code{mclapply}).
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param summary Logical, determines if the \code{summary} slot should automatically be
#'    updated by calling \code{\link[tm.plugin.koRpus:summary]{summary}} on the result.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param quiet Logical, if \code{FALSE} shows a status bar for some calculations of each text, see
#'    \code{\link[koRpus:readability]{readability}} for details.
#' @param ... options to pass through to \code{\link[koRpus:readability]{readability}}.
#' @return An object of the same class as \code{txt.file}.
#' @importFrom utils modifyList
#' @importFrom parallel mclapply
#' @importFrom koRpus readability summary split_by_doc_id
#' @include 01_class_01_kRp.corpus.R
#' @docType methods
#' @aliases readability,kRp.corpus-method
#' @rdname readability
#' @export
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_4_texts.R
#' @examples
#'
#'   myTexts <- readability(myCorpus)
#'   corpusSummary(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod(
  "readability",
  signature(txt.file="kRp.corpus"),
  function(
    txt.file,
    summary=TRUE,
    mc.cores=getOption("mc.cores", 1L),
    quiet=TRUE,
    ...
  ){
    tagged_list <- split_by_doc_id(txt.file)
    corpusReadability(txt.file) <- mclapply(names(tagged_list), function(thisText){
      dot_args <- list(...)
      if(any(!is.null(dot_args[["keep.input"]]), !is.null(dot_args[["as.feature"]]))){
        stop(simpleError("The arguments \"keep.input\" and \"as.feature\" are FALSE by default and can't be changed!"))
      } else {}
      if(any(names(corpusHyphen(txt.file)) == thisText)){
          # we probably need to drop one of two hyphen arguments if
          # readability was called from one of the wrapper functions
          default <- list(txt.file=tagged_list[[thisText]], quiet=quiet, keep.input=FALSE, ...)
          args <- modifyList(default, list(hyphen=corpusHyphen(txt.file)[[thisText]]))
          rdb <- do.call(readability, args)
      } else {
        rdb <- readability(
          tagged_list[[thisText]],
          quiet=quiet,
          keep.input=FALSE,
          as.feature=FALSE,
          ...
        )
      }
      return(rdb)
    }, mc.cores=mc.cores)
    # store meta-information on the maximum of available indices.
    # a mere summary() will simply omit NA values which can later cause
    # problems when we want to aggregate all summaries into one data.frame
    corpusMeta(txt.file, "readability") <- list(index=c())
    corpusMeta(txt.file, "readability")[["index"]] <- sort(
      unique(
        unlist(mclapply(corpusReadability(txt.file), function(thisText){
            names(summary(thisText, flat=TRUE))
          }, mc.cores=mc.cores)
        )
      )
    )
    names(corpusReadability(txt.file)) <- names(describe(txt.file))

    if(isTRUE(summary)){
      txt.file <- summary(txt.file)
    } else {}

    return(txt.file)
  }
)
