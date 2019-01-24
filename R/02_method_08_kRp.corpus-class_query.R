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

#' Apply query() to all texts in kRp.hierarchy objects
#' 
#' This method calls \code{\link[koRpus:query]{query}} on all tagged text objects
#' inside the given object (using \code{lapply}).
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.hierarchy-class]{kRp.hierarchy}}.
#' @param var A character string naming a column in the tagged text. If set to
#'    \code{"regexp"}, \code{grepl} is called on the column specified by \code{regexp_var}.
#' @param query A character vector (for words), regular expression, or single number naming values to be matched in the variable.
#'    Can also be a vector of two numbers to query a range of frequency data, or a list of named lists for multiple queries (see
#'    "Query lists" section of \code{\link[koRpus:query]{query}}).
#' @param rel A character string defining the relation of the queried value and desired results.
#'    Must either be \code{"eq"} (equal, the default), \code{"gt"} (greater than), \code{"ge"} (greater of equal),
#'    \code{"lt"} (less than) or \code{"le"} (less or equal). If \code{var="word"}, is always interpreted as \code{"eq"}
#' @param as.df Logical, if \code{TRUE}, returns a data frame, otherwise an object of the input class.
#' @param ignore.case Logical, passed through to \code{grepl} if \code{var="regexp"}.
#' @param perl Logical, passed through to \code{grepl} if \code{var="regexp"}.
#' @param regexp_var A character string naming the column to query if \code{var="regexp"}.
#' @return Depending on the arguments, might include whole objects, lists, single values etc.
#' @importMethodsFrom koRpus query
#' @keywords methods
#' @export
#' @docType methods
#' @rdname query
#' @aliases query,kRp.hierarch-method
#' @include 01_class_01_kRp.hierarchy.R
setMethod("query",
  signature(obj="kRp.hierarchy"),
  function (obj, var, query, rel="eq", as.df=TRUE, ignore.case=TRUE, perl=FALSE, regexp_var="token", mc.cores=getOption("mc.cores", 1L)){
    if(corpusLevel(obj) > 0){
      corpusChildren(obj) <- lapply(corpusChildren(obj), tm.plugin.koRpus::query, var=var, query=query, rel=rel, as.df=FALSE, ignore.case=ignore.case, perl=perl, regexp_var=regexp_var, mc.cores=mc.cores)
    } else {
      corpusTagged(obj) <- mclapply(corpusTagged(obj), function(thisText){
        query(obj=thisText, var=var, query=query, rel=rel, as.df=FALSE, ignore.case=ignore.case, perl=perl, regexp_var=regexp_var)
      }, mc.cores=mc.cores)
    }

    if(is.corpus(obj) & isTRUE(as.df)){
      obj_list <- corpusTagged(obj, level=0)
      obj_df <- lapply(obj_list, taggedText)
      result <- eval(parse(text=paste0("rbind(", paste0("obj_df[[", seq_along(obj_df), "]]", collapse= ", "), ")")))
      rownames(result) <-NULL
    } else {
      result <- obj
    }
    return(result)
  }
)
