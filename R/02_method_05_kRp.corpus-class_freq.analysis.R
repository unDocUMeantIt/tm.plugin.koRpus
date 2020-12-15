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


#' Apply freq.analysis() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:freq.analysis]{freq.analysis}} on all tagged text objects
#' inside the given \code{txt.file} object.
#' 
#' If \code{corp.freq} was not specified but a valid object of class \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}}
#' is found in the \code{freq} slot of \code{txt.file}, it is used automatically. That is the case if you called
#' \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}} on the object previously.
#' 
#' @param txt.file An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param ... options to pass through to \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#' @return An object of the same class as \code{txt.file}.
#' @importFrom koRpus freq.analysis kRp_text
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @aliases freq.analysis,kRp.corpus-method
#' @rdname freq.analysis
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   myCorpus <- read.corp.custom(myCorpus)
#'   myCorpus <- freq.analysis(myCorpus)
#'   corpusFreq(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("freq.analysis", signature(txt.file="kRp.corpus"), function(txt.file, ...){
    tagged_large <- kRp_text(
      lang=language(txt.file),
      tokens=taggedText(txt.file)
    )

    use_default_args <- TRUE
    if(is.null(list(...)[["corp.freq"]])){
      if(hasFeature(txt.file, "corp_freq")){
        default <- list(txt.file=tagged_large, ...)
        args <- modifyList(default, list(corp.freq=corpusCorpFreq(txt.file)))
        use_default_args <- FALSE
        freq_ananlysis_results <- do.call(freq.analysis, args)
      } else {}
    } else {}

    if(isTRUE(use_default_args)){
      freq_ananlysis_results <- freq.analysis(tagged_large, ...)
    } else {}

    taggedText(txt.file) <- taggedText(freq_ananlysis_results)
    corpusMeta(txt.file, meta="freq.analysis") <- describe(freq_ananlysis_results)
    corpusFreq(txt.file) <- corpusFreq(freq_ananlysis_results)
    return(txt.file)
  }
)
