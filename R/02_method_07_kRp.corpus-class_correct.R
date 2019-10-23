# Copyright 2015-2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Methods to correct kRp.corpus objects
#' 
#' These methods enable you to correct errors that occurred during automatic processing,
#' e.g., wrong hyphenation.
#' 
#' For details on what these methods do on a per text object basis, please refer to the
#' documentation of \code{\link[sylly:correct.hyph]{correct.hyph}} in the \code{sylly}
#' package.
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
# @param row Integer, the row number of the entry to be changed. Can be an integer vector
#    to change several rows in one go.
#' @param word A character string, the (possibly incorrectly hyphenated) \code{word} entry to be replaced with \code{hyphen}.
# @param tag A character string with a valid POS tag to replace the current tag entry.
#    If \code{NULL} (the default) the entry remains unchanged.
# @param lemma A character string naming the lemma to to replace the current lemma entry.
#    If \code{NULL} (the default) the entry remains unchanged.
# @param check.token A character string naming the token you expect to be in this row.
#    If not \code{NULL}, \code{correct} will stop with an error if this values don't match.
#' @param hyphen A character string, the new manually hyphenated version of \code{word}. Mustn't contain
#'    anything other than characters of \code{word} plus the hyphenation mark \code{"-"}.
#' @param cache Logical, if \code{TRUE}, the given hyphenation will be added to the sessions' hyphenation cache.
#'    Existing entries for the same word will be replaced.
#' @return An object of the same class as \code{obj}.
#' @export
#' @docType methods
#' @rdname correct
#' @aliases correct.hyph correct.hyph,kRp.corpus-method
#' @importFrom sylly correct.hyph
#' @export
#' @include 01_class_01_kRp.corpus.R
setMethod("correct.hyph",
  signature(obj="kRp.corpus"),
  function (obj, word=NULL, hyphen=NULL, cache=TRUE){
    corpusHyphen(obj) <- lapply(corpusHyphen(obj), function(thisText){
        correct.hyph(thisText, word=word, hyphen=hyphen, cache=cache)
      }
    )
    return(obj)
  }
)
