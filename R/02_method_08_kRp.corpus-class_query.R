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

#' Apply query() to all texts in kRp.corpus objects
#' 
#' This method calls \code{\link[koRpus:query]{query}} on all tagged text objects
#' inside the given object.
#' 
#' @param obj An object of class \code{\link[tm.plugin.koRpus:kRp.corpus-class]{kRp.corpus}}.
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
#' @importFrom koRpus query kRp_text
#' @include 01_class_01_kRp.corpus.R
#' @keywords methods
#' @export
#' @docType methods
#' @rdname query
#' @aliases query,kRp.hierarch-method
#' @example inst/examples/if_lang_en_clause_start.R
#' @example inst/examples/generate_myCorpus_2_texts.R
#' @examples
#'
#'   query(myCorpus, var="lttr", query="7", rel="gt")
#' @example inst/examples/if_lang_en_clause_end.R
setMethod("query",
  signature(obj="kRp.corpus"),
  function (obj, var, query, rel="eq", as.df=TRUE, ignore.case=TRUE, perl=FALSE, regexp_var="token"){
    tagged_large <- kRp_text(
      lang=language(obj),
      tokens=taggedText(obj)
    )
    obj <- query(obj=tagged_large, var=var, query=query, rel=rel, as.df=FALSE, ignore.case=ignore.case, perl=perl, regexp_var=regexp_var)

    if(all(is.corpus(obj), isTRUE(as.df))){
      result <- taggedText(obj)
      rownames(result) <-NULL
    } else {
      result <- obj
    }
    return(result)
  }
)
