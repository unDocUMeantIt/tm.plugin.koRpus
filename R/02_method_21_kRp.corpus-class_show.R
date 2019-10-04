# Copyright 2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Show methods for kRp.flatHier objects
#'
#' Show methods for S4 objects of class \code{\link[tm.plugin.koRpus:kRp.flatHier-class]{kRp.flatHier}}.
#'
#' @param object An object of class \code{kRp.flatHier}.
#' @export
#' @docType methods
#' @aliases show,kRp.flatHier-method
#' @rdname show
#' @include 01_class_02_kRp.flatHier.R
setMethod("show", signature(object="kRp.flatHier"), function(object){
  num_texts <- length(levels(taggedText(object)[["doc_id"]]))
  if(num_texts != 1){
    txt <- "texts"
  } else {
    txt <- "text"
  }

  message(paste0("A corpus with ", num_texts, " ", txt, "." ))
  
  hierarchy <- slot(object, "hierarchy")
  txt_hier <- "\nThe texts are "
  if(length(hierarchy) > 0){
    txt_hier <- paste0(txt_hier, " hierarchically grouped:\n\n  ")
    groups <- paste0(sapply(
      names(hierarchy),
      function(this_hier){
        IDs <- hierarchy[[this_hier]]
        return(paste0(this_hier, ":\n    \"", paste0(IDs, collapse="\", \""), "\""))
      }
    ), collapse="\n  ")
    txt_hier <- paste0(txt_hier, groups)
  } else {
    txt_hier <- paste0(txt_hier, "not hierarchically grouped.")
  }
  
  message(txt_hier)
  
})
