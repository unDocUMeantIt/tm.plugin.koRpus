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


#' Apply textTransform() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:textTransform]{textTransform}} on all tagged text objects
#' inside the given \code{txt} object (using \code{mclapply}).
#' 
#' @param txt An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:textTransform]{textTransform}}.
#' @return An object of the same class as \code{txt}.
#' @importFrom parallel mclapply
#' @importFrom koRpus textTransform
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases textTransform,kRp.corpus-method
#' @rdname textTransform
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   head(taggedText(myCorpus), n=10)
#'   myCorpus <- textTransform(myCorpus, scheme="minor")
#'   head(taggedText(myCorpus), n=10)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("textTransform", signature(txt="kRp.corpus"), function(txt, mc.cores=getOption("mc.cores", 1L), ...){
    return(
      text_transform_wrapper(
        obj=txt,
        trans_method=koRpus::textTransform,
        mc.cores=mc.cores,
        ...
      )
    )
  }
)

## function text_transform_wrapper()
# this wrapper can be called by all methods invoking text transformation,
# e.g. textTransform(), cTest(), jumbleWords(), or clozeDelete()
# - obj: an object of class kRp.corpus
# - trans_method: an object of class function
#' @importFrom koRpus split_by_doc_id
text_transform_wrapper <- function(obj, trans_method, mc.cores=getOption("mc.cores", 1L), ...){
    tagged_list <- split_by_doc_id(obj)
    transformed_texts <- mclapply(tagged_list, trans_method, ..., mc.cores=mc.cores)
    corpusTagged_df <- do.call(rbind, mclapply(transformed_texts, taggedText, mc.cores=mc.cores))
    row.names(corpusTagged_df) <- NULL
    taggedText(obj) <- corpusTagged_df
    describe(obj) <- mclapply(transformed_texts, describe, mc.cores=mc.cores)
    diffText(obj) <- mclapply(transformed_texts, diffText, mc.cores=mc.cores)
    return(obj)
} ## end function text_transform_wrapper()
