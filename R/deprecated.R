# Copyright 2018-2019 Meik Michalke <meik.michalke@hhu.de>
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

#' Deprecated functions and methods
#' 
#' These functions were used in earlier versions of the package but either
#' replaced or removed.
#'
#' @param obj No longer used.
#' @param ... No longer used.
#' @rdname deprecated
#' @aliases tm.plugin.koRpus-deprecated
#' @export

corpusTagged <- function(obj, ...){
  .Deprecated(new="taggedText")
  return(taggedText(obj=obj, ...))
}

#' @rdname deprecated
#' @aliases tm.plugin.koRpus-deprecated
#' @export
corpusTTR <- function(obj, ...){
  .Deprecated(new="corpusLexDiv")
  return(corpusLexDiv(obj=obj, ...))
}

#' @rdname deprecated
#' @aliases tm.plugin.koRpus-defunct
#' @export
corpusLevel <- function(...){.Defunct(new="", msg="")}

#' @rdname deprecated
#' @aliases tm.plugin.koRpus-defunct
#' @export
corpusCategory <- function(...){.Defunct(msg="This method is no longer used because the object structure was changed.")}

#' @rdname deprecated
#' @aliases tm.plugin.koRpus-defunct
#' @export
corpusID <- function(...){.Defunct(msg="This method is no longer used because the object structure was changed.")}

#' @rdname deprecated
#' @aliases tm.plugin.koRpus-defunct
#' @export
corpusPath <- function(...){.Defunct(msg="This method is no longer used because the object structure was changed.")}
