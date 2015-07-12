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


#' S4 Class kRp.topicCorpus
#'
#' Objects of this class can contain multiple texts simultaneously. Adding to that, these texts can be ordered
#' at two levels: topic and source. This is useful for comparisons if you have a defined number of topics and
#' textst from different sources on these topics. Note however, that there must be at least one text on each topic
#' in all of the given sources.
#' 
#' For each combination of topic and source, there is exactly one object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
#' 
#' @slot summary A summary data.frame for all topics combined.
#' @slot topics A named list of nested objects. Each element is named after a topic and contains an object of class kRp.sourcesCorpus.
#' @slot freq An object of class \code{\link[koRpus]{kRp.corp.freq-class}}, can contain word frequency
#'  information on the full corpus if this object was analysed with
#'  \code{\link[tm.plugin.koRpus:read.corp.custom]{read.corp.custom}}.
#' @name kRp.topicCorpus,-class
#' @aliases kRp.topicCorpus,-class kRp.topicCorpus-class
#' @import methods
#' @keywords classes
#' @export
#' @include 01_class_02_kRp.sourcesCorpus.R
#' @rdname kRp.topicCorpus-class
setClass("kRp.topicCorpus",
    representation=representation(
    summary="data.frame",
    topics="list",
    freq="kRp.corp.freq"),
  prototype=prototype(
    summary=data.frame(),
    topics=list(),
    freq=new("kRp.corp.freq")
  )
)

setValidity("kRp.topicCorpus", function(object){
    topics <- slot(object, "topics")

    for (thisTopic in names(topics)) {
      if(!inherits(topics[[thisTopic]], "kRp.sourcesCorpus")){
        stop(simpleError(paste0("Invalid object: Slot \"", thisTopic, "\" must have entries inheriting from class kRp.sourcesCorpus!")))
      } else {}
    }
    return(TRUE)
})
