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


#' Turn a kRp.corpus object into a list of kRp.tagged objects
#' 
#' For some analysis steps it might be important to have individual tagged texts
#' instead of one large corpus object. This method produces just that.
#'
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' @return A named list of objects of class \code{\link[koRpus:kRp.tagged-class]{kRp.tagged}}.
#'    Elements are named by their \code{doc_id}.
#' @export
#' @docType methods
#' @rdname corpus2tagged
#' @examples
#' \dontrun{
#' myCorpus <- readCorpus(
#'   dir=file.path(path.package("tm.plugin.koRpus"), "tests", "testthat", "samples"),
#'   hierarchy=list(
#'     Topic=c(
#'       C3S="C3S",
#'       GEMA="GEMA"
#'     ),
#'     Source=c(
#'       Wikipedia_alt="Wikipedia (alt)",
#'       Wikipedia_neu="Wikipedia (neu)"
#'     )
#'   )
#' )
#' 
#' myCorpusList <- corpus2tagged(myCorpus)
#' }
setGeneric("corpus2tagged", function(obj) standardGeneric("corpus2tagged"))

#' @rdname corpus2tagged
#' @docType methods
#' @export
#' @aliases
#'    corpus2tagged,-methods
#'    corpus2tagged,kRp.corpus-method
#' @include 01_class_01_kRp.corpus.R
setMethod("corpus2tagged",
  signature=signature(obj="kRp.corpus"),
  function(obj){
    tt_desc <- describe(obj)
    tt_lang <- language(obj)
    tt_tagged <- taggedText(obj)
    tt_list <- split(tt_tagged, tt_tagged[["doc_id"]])
    if(hasFeature(obj, "diff")){
      result <- lapply(
        names(tt_list),
        function(thisText){
          kRp_txt_trans(
            lang=tt_lang,
            desc=tt_desc[[thisText]],
            tokens=tt_list[[thisText]],
            diff=diffText(obj)[[thisText]]
          )
        }
      )
    } else {
      result <- lapply(
        names(tt_list),
        function(thisText){
          kRp_tagged(
            lang=tt_lang,
            desc=tt_desc[[thisText]],
            tokens=tt_list[[thisText]]
          )
        }
      )
    }
    names(result) <- names(tt_desc)
    return(result)
  }
) ## end method corpus2tagged()
