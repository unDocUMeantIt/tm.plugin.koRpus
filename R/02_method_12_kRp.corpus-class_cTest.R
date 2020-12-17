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


#' Apply cTest() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:cTest]{cTest}} on all tagged text objects
#' inside the given \code{obj} object (using \code{mclapply}).
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param ... options to pass through to \code{\link[koRpus:cTest]{cTest}}.
#' @return An object of the same class as \code{obj}.
#' @importFrom parallel mclapply
#' @importFrom koRpus cTest
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases cTest,kRp.corpus-method
#' @rdname cTest
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   taggedText(myCorpus)[20:30,]
#'   myCorpus <- cTest(myCorpus)
#'   taggedText(myCorpus)[20:30,]
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("cTest", signature(obj="kRp.corpus"), function(obj, mc.cores=getOption("mc.cores", 1L), ...){
    return(
      # text_transform_wrapper() is defined in 02_method_13_kRp.corpus-class_textTransform.R
      text_transform_wrapper(
        obj=obj,
        trans_method=koRpus::cTest,
        mc.cores=mc.cores,
        ...
      )
    )
  }
)
