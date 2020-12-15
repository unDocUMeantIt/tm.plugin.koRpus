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


#' Turn a kRp.corpus object into a list of kRp.text objects
#' 
#' For some analysis steps it might be important to have individual tagged texts
#' instead of one large corpus object. This method produces just that.
#'
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @param keepFeatures Either logical, whether to keep all features or drop them, or a character vector
#'    of names of features to keep if present.
#' @return A named list of objects of class \code{\link[koRpus:kRp.text-class]{kRp.text}}.
#'    Elements are named by their \code{doc_id}.
#' @importFrom koRpus split_by_doc_id kRp_text
#' @include 01_class_01_kRp.corpus.R
#' @export
#' @docType methods
#' @rdname split_by_doc_id
#' @aliases
#'    split_by_doc_id,-methods
#'    split_by_doc_id,kRp.corpus-method
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_4_texts.R
#' @examples
#'
#'   myCorpusList <- split_by_doc_id(myCorpus)
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("split_by_doc_id",
  signature=signature(obj="kRp.corpus"),
  function(obj, keepFeatures=TRUE){
    return(
      split_by_doc_id(
        kRp_text(
          lang=language(obj),
          desc=describe(obj),
          tokens=taggedText(obj),
          features=slot(obj, "features"),
          feat_list=slot(obj, "feat_list")
        ),
        keepFeatures=keepFeatures
      )
    )
  }
) ## end method split_by_doc_id()
