# Copyright 2015 Meik Michalke <meik.michalke@hhu.de>
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


#' S4 Class kRp.sourcesCorpus
#'
#' Objects of this class can contain multiple texts simultaneously. Adding to that, these texts are ordered
#' by their source in a number of slots. Each slot is a list of entries, one for each source.
#' 
#' @slot summary A summary data.frame for all sources combined.
#' @slot paths A list of character strings with paths to all files
#' @slot sources A list of objects of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}
#' @slot files A list of character strings with only the file names of all texts
#' @slot freq An object of class \code{\link[koRpus]{kRp.corp.freq-class}}, can contain word frequency
#'  information on the full corpus if this object was analysed with
#'  \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}}.
#' @name kRp.sourcesCorpus,-class
#' @aliases kRp.sourcesCorpus,-class kRp.sourcesCorpus-class
#' @import methods koRpus
#' @keywords classes
#' @export
#' @include 01_class_01_kRp.corpus.R
#' @rdname kRp.sourcesCorpus-class
setClass("kRp.sourcesCorpus",
    representation=representation(
    summary="data.frame",
    paths="list",
    sources="list",
    files="list",
    freq="kRp.corp.freq"),
  prototype=prototype(
    summary=data.frame(),
    paths=list(),
    sources=list(),
    files=list(),
    freq=new("kRp.corp.freq")
  )
)

setValidity("kRp.sourcesCorpus", function(object){
    paths <- slot(object, "paths")
    sources <- slot(object, "sources")
    files <- slot(object, "files")

    if(!all(
          identical(names(summary), names(paths)),
          identical(names(paths), names(sources)),
          identical(names(sources), names(files))
        )
      ){
      stop(simpleError("All slots except \"freq\" must have entries of the same name!"))
    }
    
    for (thisPath in names(paths)) {
      if(!inherits(paths[[thisPath]], "character")){
        stop(simpleError(paste0("Invalid object: Slot \"", thisPath, "\" must have entries inheriting from class character!")))
      } else {}
    }
    for (thisSource in names(sources)) {
      if(!inherits(sources[[thisSource]], "kRp.corpus")){
        stop(simpleError(paste0("Invalid object: Slot \"", thisSource, "\" must have entries inheriting from class kRp.corpus!")))
      } else {}
    }
    for (thisFile in names(files)) {
      if(!inherits(files[[thisFile]], "character")){
        stop(simpleError(paste0("Invalid object: Slot \"", thisFile, "\" must have entries inheriting from class character!")))
      } else {}
    }
    return(TRUE)
})
