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
#' This method prints a \code{\linkS4class{AbstractMassObject}} object 
#' to a file or connection.
#' 
#' @param x a \code{\linkS4class{AbstractMassObject}} object
#' @param file filename
#' @param row.names either a logical value indicating whether the row names of
#'  ‘x’ are to be written along with ‘x’, or a character vector
#'  of row names to be written.
#' @param col.names either a logical value indicating whether the column names
#'  of ‘x’ are to be written along with ‘x’, or a character
#'  vector of column names to be written.  See the section on
#'  ‘CSV files’ for the meaning of ‘col.names = NA’.
#' @param ... further arguments passed to \code{\link{write.table}}
#'
#' @seealso \code{\linkS4class{AbstractMassObject}}, \code{\link{write.table}}
#' @aliases write.table,AbstractMassObject-method
#' @docType methods
#' @rdname write.table-methods
#' @importFrom utils write.table
#' @exportMethod write.table
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

#' Data output
#' 
#' This method prints a \code{\linkS4class{AbstractMassObject}} object 
#' to a file or connection (in csv format).
#' 
#' @param x a \code{\linkS4class{AbstractMassObject}} object
#' @param file filename
#' @param row.names either a logical value indicating whether the row names of
#'  ‘x’ are to be written along with ‘x’, or a character vector
#'  of row names to be written.
#' @param col.names either a logical value indicating whether the column names
#'  of ‘x’ are to be written along with ‘x’, or a character
#'  vector of column names to be written.  See the section on
#'  ‘CSV files’ for the meaning of ‘col.names = NA’.
#' @param sep the field separator string.
#' @param ... further arguments passed to \code{\link{write.table}}
#'
#' @seealso \code{\linkS4class{AbstractMassObject}}, \code{\link{write.table}}
#' @aliases write.table,AbstractMassObject-method
#' @docType methods
#' @rdname write.table-methods
#' @importFrom utils write.csv
#' @exportMethod write.csv
setMethod(f="write.csv",
  signature=signature("AbstractMassObject"),
  definition=function(x, file="", row.names=FALSE, col.names=TRUE, sep=",",
                      ...) {
  return(write.table(x, file=file, row.names=row.names, col.names=col.names,
                     sep=sep, ...))
})

