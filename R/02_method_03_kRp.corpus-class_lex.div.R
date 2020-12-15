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


#' Apply lex.div() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:lex.div]{lex.div}} on all tagged text objects
#' inside the given \code{txt} object (using \code{mclapply}).
#' 
#' @param txt An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param summary Logical, determines if the \code{summary} slot should automatically be
#'    updated by calling \code{\link[tm.plugin.koRpus:summary]{summary}} on the result.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param char Character vector to specify measures of which characteristics should be computed, see
#'    \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @param quiet Logical, if \code{FALSE} shows a status bar for some measures of each text, see
#'    \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @param ... options to pass through to \code{\link[koRpus:lex.div]{lex.div}}.
#' @return An object of the same class as \code{txt}.
#' @importFrom parallel mclapply
#' @importFrom koRpus summary lex.div split_by_doc_id
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases lex.div,kRp.corpus-method
#' @rdname lex.div
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_4_texts.R
#' @examples
#'   myCorpus <- lex.div(myCorpus)
#'   corpusSummary(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("lex.div", signature(txt="kRp.corpus"), function(txt, summary=TRUE, mc.cores=getOption("mc.cores", 1L), char="", quiet=TRUE, ...){
    dot_args <- list(...)
    if(!is.null(dot_args[["as.feature"]])){
      stop(simpleError("The argument \"as.feature\" is FALSE by default and can't be changed!"))
    } else {}
    tagged_list <- split_by_doc_id(txt)
    corpusLexDiv(txt) <- mclapply(names(describe(txt)), function(thisText){
      lex.div(tagged_list[[thisText]], char=char, quiet=quiet, ...)
    }, mc.cores=mc.cores)
    # store meta-information on the maximum of available indices.
    # a mere summary() will simply omit NA values which can later cause
    # problems when we want to aggregate all summaries into one data.frame
    corpusMeta(txt, "lex_div") <- list(index=c())
    corpusMeta(txt, "lex_div")[["index"]] <- sort(
      unique(
        unlist(mclapply(corpusLexDiv(txt), function(thisText){
            return(names(summary(thisText, flat=TRUE)))
          }, mc.cores=mc.cores)
        )
      )
    )
    names(corpusLexDiv(txt)) <- names(describe(txt))

    if(isTRUE(summary)){
      txt <- summary(txt)
    } else {}

    return(txt)
  }
)
