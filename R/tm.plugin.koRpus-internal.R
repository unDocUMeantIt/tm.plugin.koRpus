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

## function fixMissingIndices()
# used by summary() to not fail because of missing values in single texts
# have: the actual named summary vector
# want: character vector with the names of indices expected
fixMissingIndices <- function(have, want, missing=NA){
  miss.index <- want[!want %in% names(have)]
  have[miss.index] <- missing
  return(have[order(names(have))])
} ## end function fixMissingIndices()


## function nullToList()
# checks for NULL values and returns them as empty list,
# otherwise uniqe and sorted
nullToList <- function(obj, entry="index"){
  if(!is.null(obj)){
    if(!is.null(entry)){
      obj <- list(sort(unique(obj)))
      names(obj) <- entry
    } else {
      obj <- sort(unique(obj))
    }
  } else {
    obj <- list()
  }
  return(obj)
} ## end function nullToList()


## function whatIsAvailable()
whatIsAvailable <- function(all.corpora){
  # this fetches all level 0 objects from the nested corpus
  # result is a list
  bottom_level <- corpusChildren(all.corpora, level=0)
  # now get all availability info from that list
  available.rdb <- unique(unlist(lapply(
    bottom_level,
    function(thisCorpus){
      unlist(corpusMeta(thisCorpus, "readability", fail=FALSE)[["index"]])
    }
  )))
  available.TTR <- unique(unlist(lapply(
    bottom_level,
    function(thisCorpus){
      unlist(corpusMeta(thisCorpus, "TTR", fail=FALSE)[["index"]])
    }
  )))
  available.rdb <- nullToList(available.rdb, entry="index")
  available.TTR <- nullToList(available.TTR, entry="index")
  return(list(available.rdb=available.rdb, available.TTR=available.TTR))
} ## end function whatIsAvailable()


## function availableFromOptions()
availableFromOptions <- function(allOptions, object){
  if("available.rdb" %in% names(allOptions)){
    available.rdb <- allOptions[["available.rdb"]][["index"]]
  } else {
    if(is.list(object)){
      available.rdb <- object[["available.rdb"]][["index"]]
    } else {
      available.rdb <- corpusMeta(object, "readability", fail=FALSE)[["index"]]
    }
  }
  if("available.TTR" %in% names(allOptions)){
    available.TTR <- allOptions[["available.TTR"]][["index"]]
  } else {
    if(is.list(object)){
      available.TTR <- object[["available.TTR"]][["index"]]
    } else {
      available.TTR <- corpusMeta(object, "TTR", fail=FALSE)[["index"]]
    }
  }
  available.rdb <- nullToList(available.rdb, entry="index")
  available.TTR <- nullToList(available.TTR, entry="index")
  return(list(available.rdb=available.rdb, available.TTR=available.TTR))
} ## end function availableFromOptions()


## function getRdbDesc()
getRdbDesc <- function(object){
  rdbDesc <- t(as.data.frame(
    sapply(corpusReadability(object), function(thisText){
      sylls <- unlist(describe(thisText)[["syllables"]]["all"])
      sylls <- ifelse(is.null(sylls), NA, sylls["all"])
      result <- c(
        sentences=describe(thisText)[["sentences"]],
        words=describe(thisText)[["words"]],
        letters=describe(thisText)[["letters"]]["all"],
        syllables=sylls,
        punct=describe(thisText)[["punct"]],
        avg.sentc.length=describe(thisText)[["avg.sentc.length"]],
        avg.word.length=describe(thisText)[["avg.word.length"]],
        avg.syll.word=describe(thisText)[["avg.syll.word"]],
        sntc.per.word=describe(thisText)[["sntc.per.word"]],
        TTR=describe(thisText)[["TTR"]]
      )
    })
  ))
  return(rdbDesc)
} ## end function getRdbDesc()


## function hierarchy_from_dirtree()
hierarchy_from_dirtree <- function(
  dir, level=0, result=list()
){
  subdirs <- list.dirs(dir, full.names=FALSE, recursive=FALSE)
  if(length(subdirs) > 0){
    subsubdirs <- unique(lapply(file.path(dir, subdirs), list.dirs, full.names=FALSE, recursive=FALSE))
    if(length(subsubdirs) > 1){
      stop(simpleError("Can't auto-detect hierarchy structure: subdirectories do not match!"))
    } else {}
    names(subdirs) <- subdirs
    result[[paste0("level", level)]] <- subdirs
    result <- hierarchy_from_dirtree(dir=file.path(dir, subdirs[1]), level=level + 1, result=result)
  } else {}
  return(result)
} ## end function hierarchy_from_dirtree()
