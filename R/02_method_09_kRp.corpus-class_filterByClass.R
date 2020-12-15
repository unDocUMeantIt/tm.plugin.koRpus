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


#' Apply filterByClass() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:filterByClass]{filterByClass}} on all tagged text objects
#' inside the given \code{txt} object (using \code{mclapply}).
#' 
#' @param txt An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:filterByClass]{filterByClass}}.
#' @return An object of the same class as \code{txt}.
#' @importFrom parallel mclapply
#' @importFrom koRpus filterByClass split_by_doc_id
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases filterByClass,kRp.corpus-method
#' @rdname filterByClass
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   head(taggedText(myCorpus), n=10)
#'   # remove all punctuation
#'   myCorpus <- filterByClass(myCorpus)
#'   head(taggedText(myCorpus), n=10)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("filterByClass", signature(txt="kRp.corpus"), function(txt, mc.cores=getOption("mc.cores", 1L), ...){
    # filterByClass() by default updates the desc slot, and since that is still split into
    # individual texts, we must get individual calculations and merge the tokens data frames
    # again afterwards
    tagged_list <- split_by_doc_id(txt)
    filtered_list <- mclapply(tagged_list, function(thisText){
      filterByClass(thisText, ...)
    }, mc.cores=mc.cores)
    corpusTagged_df <- do.call(rbind, mclapply(filtered_list, taggedText, mc.cores=mc.cores))
    row.names(corpusTagged_df) <- NULL
    taggedText(txt) <- corpusTagged_df
    describe(txt) <- mclapply(filtered_list, describe, mc.cores=mc.cores)
    return(txt)
  }
)
