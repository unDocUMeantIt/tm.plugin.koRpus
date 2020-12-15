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

#' Get a comprehensive data frame describing the files of your corpus
#' 
#' The function translates the hierarchy defintion given into a data frame with
#' one row for each file, including the generated document ID.
#' 
#' @param dir File path to the root directory of the text corpus, or a TIF[1] compliant data frame.
#' @param hierarchy A named list of named character vectors describing the directory hierarchy level by level.
#'    If \code{TRUE} instead, the hierarchy structure is taken directly from the directory tree.
#'    See section Hierarchy of \code{\link[tm.plugin.koRpus:readCorpus]{readCorpus}} for details.
#' @param fsep Character string defining the path separator to use.
#' @param full_list Logical, see return value.
#' @return Either a data frame with columns \code{doc_id}, \code{file}, \code{path} and one further factor
#'    column for each hierarchy level, or (if \code{full_list=TRUE}) a list containing that data frame
#'    (\code{all_files}) and also data frames describing the hierarchy by given names (\code{hier_names}),
#'    directories (\code{hier_dirs}) and relative paths (\code{hier_paths}).
#' @references
#'    [1] Text Interchange Formats (\url{https://github.com/ropensci/tif})
#' @export
#' @examples
#' myCorpusFiles <- corpus_files(
#'   dir=file.path(
#'     path.package("tm.plugin.koRpus"), "examples", "corpus"
#'   ),
#'   hierarchy=list(
#'     Topic=c(
#'       Winner="Reality Winner",
#'       Edwards="Natalie Edwards"
#'     ),
#'     Source=c(
#'       Wikipedia_prev="Wikipedia (old)",
#'       Wikipedia_new="Wikipedia (new)"
#'     )
#'   )
#' )
corpus_files <- function(
  dir,
  hierarchy=list(),
  fsep=.Platform$file.sep,
  full_list=FALSE
){
  if(length(hierarchy) > 0){
    # generate a data frame listing all path combinations to expect
    # expand.grid() actually returns the reverse order we want, but
    # we'll fix that later simply by sorting all generated paths
    hier_names <- expand.grid(
      hierarchy,
      KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE
    )
    hier_dirs <- expand.grid(
      lapply(hierarchy, names),
      KEEP.OUT.ATTRS=FALSE,
      stringsAsFactors=FALSE
    )
    hier_paths <- apply(
      hier_dirs,
      MARGIN=1,
      paste0,
      collapse=.Platform$file.sep
    )
    hier_order <- order(hier_paths)
    hier_names <- hier_names[hier_order,, drop=FALSE]
    hier_dirs <- hier_dirs[hier_order,, drop=FALSE]
    hier_paths <- hier_paths[hier_order]
  } else {
    hier_names <- hier_dirs <- data.frame()
    hier_paths <- ""
  }

  if(is.data.frame(dir)){
    all_cols <- c("doc_id", "file", "path", colnames(hier_dirs))
    available_cols <- all_cols[all_cols %in% colnames(dir)]
    all_files <- dir[, available_cols, drop=FALSE]
  } else {
    full_hier_paths <- normalizePath(file.path(dir, hier_paths), mustWork=TRUE)

    all_files <- as.data.frame(
      matrix(
        character(),
        ncol=3+ncol(hier_dirs),
        dimnames=list(
          c(),
          c("doc_id", "file", "path", colnames(hier_dirs))
        )
      ),
      stringsAsFactors=FALSE
    )
    for (thisPath in seq_along(hier_paths)){
      hier_files <- list.files(full_hier_paths[thisPath])
      this_cat <- hier_dirs[thisPath,]
      row.names(this_cat) <- NULL
      append_files <- data.frame(
        doc_id=gsub(
          "[^[:alnum:]_\\\\.-]+", "",
            gsub(
              "[[:space:]]+", "_",
              apply(cbind(this_cat, hier_files), 1, paste0, collapse="-")
            )
          ),
        file=hier_files,
        path=full_hier_paths[thisPath],
        stringsAsFactors=FALSE
      )
      append_files[,colnames(hier_dirs)] <- hier_names[thisPath,]
      all_files <- rbind(all_files, append_files)
    }
  }
  for (thisCat in colnames(hier_dirs)){
    all_files[[thisCat]] <- as.factor(all_files[[thisCat]])
  }

  if(isTRUE(full_list)){
    return(list(
      all_files=all_files,
      hier_names=hier_names,
      hier_dirs=hier_dirs,
      hier_paths=hier_paths
    ))
  } else {
    return(all_files)
  }
}
