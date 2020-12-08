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


#' A source function for tm
#'
#' An rather untested attempt to sketch a \code{\link[tm:Source]{Source}} function for \code{tm}.
#' Supposed to be used to translate tagged \code{koRpus} objects into \code{tm} objects.
#' 
#' Also provided are the methods \code{getElem} and \code{pGetElem} for S3 class \code{kRpSource}.
#' 
#' @param obj An object of class \code{\link[koRpus:kRp.text-class]{kRp.text}} (a class union for tagged text objects).
#' @param encoding Character string, defining the character encoding of the object.
#' @return An object of class \code{\link[tm:Source]{Source}}, also inheriting class \code{kRpSource}.
#' @importFrom tm SimpleSource readPlain
#' @importFrom koRpus kRp.text.paste
#' @export
kRpSource <- function(obj, encoding="UTF-8"){
  thisText <- kRp.text.paste(obj)
  s <- SimpleSource(
    reader=readPlain,
    encoding=encoding,
    length=length(thisText),
    names=names(thisText),
    class="kRpSource")
  s$Content <- thisText
  s$Language <- language(obj)
  return(s)
}

getElem <- function(x) UseMethod("getElem", x) 
getElem.kRpSource <- function(x){
  list(content = x$Content[x$Position], uri = NA, language=x$Language)
}

pGetElem <- function(x) UseMethod("pGetElem", x) 
pGetElem.kRpSource <- function(x){
  lapply(x$Content, function(y) list(content = y, uri = NA))
}
