### Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

#' Data output
#' 
#' This methods prints a \code{\linkS4class{AbstractMassObject}} object 
#' to a file or connection.
#' 
#' @usage write.table(x, file="", append=FALSE, quote=TRUE, sep=" ", eol="\n",
#'                    na="NA", dec=".", row.names=FALSE, col.names=FALSE, 
#'                    qmethod=c("escape", "double"), fileEncoding="")
#' @usage write.csv(x, file="", row.names=FALSE, col.names=TRUE, sep=",", ...)
#' @param x a \code{\linkS4class{AbstractMassObject}} object
#' @param file filename
#' @param row.names either a logical value indicating whether the row names of
#'  \code{x} are to be written along with \code{x}, or a character vector
#'  of row names to be written.
#' @param col.names either a logical value indicating whether the column names
#'  of \code{x} are to be written along with \code{x}, or a character
#'  vector of column names to be written.  See the section on
#'  \code{CSV files} for the meaning of \code{col.names = NA}.
#' @param sep the field separator string.
#' @param path directory path
#' @param force create \code{path} if not exists
#' @param ending file type (e.g. "txt", "csv", ...)
#' @param ... further arguments passed to \code{\link{write.table}}
#'
#' @seealso \code{\linkS4class{AbstractMassObject}}, \code{\link{write.table}}
#' @aliases write.table,AbstractMassObject-method write.table,list-method
#'  export export,AbstractMassObject-method export,list-method
#'  write.csv write.csv,AbstractMassObject-method write.csv,list-method
#'  exportCsv exportCsv,AbstractMassObject-method exportCsv,list-method
#' @docType methods
#' @keywords methods
#' @rdname write.table-methods
#' @importFrom utils write.table write.csv
#' @exportMethod write.table
#' @exportMethod write.csv
#' @exportMethod export
#' @exportMethod exportCsv
#' @examples
#' \dontrun{
#' s <- createMassSpectrum(mass=1:5, intensity=6:10,
#'                         metaData=list(fullName="sample1")
#' write.csv(s, "/home/user/msdata/")
#' }
setMethod(f="write.table",
  signature=signature(x="AbstractMassObject"),
  definition=function(x, file="", append=FALSE, quote=TRUE, sep=" ", eol="\n",
                      na="NA", dec=".", row.names=FALSE, col.names=FALSE, 
                      qmethod=c("escape", "double"), fileEncoding="") {
  return(write.table(as.matrix(x), file=file, append=append, quote=quote,
                     sep=sep, eol=eol, na=na, dec=dec, row.names=row.names,
                     col.names=col.names, qmethod=qmethod,
                     fileEncoding=fileEncoding))
})

setMethod(f="export",
  signature=signature(x="AbstractMassObject"),
  definition=function(x, ...) {
  return(write.table(x, ...))
})

setMethod(f="export",
  signature=signature(x="list"),
  definition=function(x, path, force=TRUE, ending="txt", ...) {
    MALDIquant:::.stopIfNotMassObjectList(x);

    if (!file.exists(path) && force) {
      dir.create(path, showWarnings=FALSE, recursive=TRUE)  
    }

    if (!file.info(path)$isdir) {
      stop(sQuote(path), " is no directory!")
    }

    ## stop if directory isn't writeable
    if (file.access(path, 2) != 0) {
      stop("No permissions to write into ", sQuote(path), "!")
    }

    invisible(lapply(x, function(y) {
      return(write.table(x=y, 
                         file=file.path(path, 
                                        .composeFilename(y, ending=ending)),
                         ...))
    }))
})

setMethod(f="write.csv",
  signature=signature("AbstractMassObject"),
  definition=function(x, file="", row.names=FALSE, col.names=TRUE, sep=",", ...) {
    return(write.table(x=x, file=file, row.names=row.names, col.names=col.names, 
                       sep=sep, ...))
})

setMethod(f="write.csv",
  signature=signature("list"),
  definition=function(x, path, force=TRUE, row.names=FALSE, col.names=TRUE,
                      sep=",", ending="csv", ...) {
    return(export(x=x, path=path, force=force, row.names=row.names,
                  col.names=col.names, sep=sep, ending=ending, ...))
})

setMethod(f="exportCsv",
  signature=signature(x="list"),
  definition=function(x, ...) {
  invisible(write.csv(x, ...))
})

setMethod(f="exportCsv",
  signature=signature("AbstractMassObject"),
  definition=function(x, ...) {
  return(write.csv(x, ...))
})

