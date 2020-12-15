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


#' Apply jumbleWords() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:jumbleWords]{jumbleWords}} on all tagged text objects
#' inside the given \code{words} object (using \code{mclapply}).
#' 
#' @param words An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:jumbleWords]{jumbleWords}}.
#' @return An object of the same class as \code{words}.
#' @importFrom parallel mclapply
#' @importFrom koRpus jumbleWords
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases jumbleWords,kRp.corpus-method
#' @rdname jumbleWords
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   head(taggedText(myCorpus), n=10)
#'   myCorpus <- jumbleWords(myCorpus)
#'   head(taggedText(myCorpus), n=10)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("jumbleWords", signature(words="kRp.corpus"), function(words, mc.cores=getOption("mc.cores", 1L), ...){
    return(
      # text_transform_wrapper() is defined in 02_method_13_kRp.corpus-class_textTransform.R
      text_transform_wrapper(
        obj=words,
        trans_method=koRpus::jumbleWords,
        mc.cores=mc.cores,
        ...
      )
    )
  }
)
