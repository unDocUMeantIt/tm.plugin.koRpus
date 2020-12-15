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


#' Apply hyphen() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[sylly:hyphen]{hyphen}} on all tagged text objects
#' inside the given \code{words} object (using \code{mclapply}).
#' 
#' @param words An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param mc.cores The number of cores to use for parallelization, see \code{\link[parallel:mclapply]{mclapply}}.
#' @param quiet Logical, if \code{FALSE} shows a status bar for the hyphenation process of each text.
#' @param ... options to pass through to \code{\link[sylly:hyphen]{hyphen}}.
#' @return An object of the same class as \code{words}.
#' @importFrom parallel mclapply
#' @importFrom sylly hyphen
#' @importFrom koRpus split_by_doc_id
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases hyphen,kRp.corpus-method
#' @rdname hyphen
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_1_text.R
#' @examples
#'
#'   myCorpus <- hyphen(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod(
  "hyphen",
  signature(words="kRp.corpus"),
  function(
    words,
    mc.cores=getOption("mc.cores", 1L),
    quiet=TRUE,
    ...
  ){
    tagged_list <- split_by_doc_id(words)
    corpusHyphen(words) <- mclapply(tagged_list, function(thisText){
      hyphen(thisText, quiet=quiet, ...)
    }, mc.cores=mc.cores)
    return(words)
  }
)
