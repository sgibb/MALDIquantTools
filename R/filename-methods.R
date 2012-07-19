## Copyright 2012 Sebastian Gibb
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

#' This method creates a filename for a \code{\linkS4class{AbstractMassObject}}
#' object.
#' 
#' @param x a \code{\linkS4class{AbstractMassObject}} object
#' @param ending file typ (e.g. "txt", "pdf", ...)
#' @return filename 
#'
#' @seealso \code{\linkS4class{AbstractMassObject}}
#' @keywords internal
#' @aliases .composeFilename,AbstractMassObject-method
#' @docType methods
#' @rdname composeFilename-methods
setMethod(f=".composeFilename",
  signature=signature(x="AbstractMassObject"),
  definition=function(x, ending="txt") {

  if (!is.null(metaData(x)$fullName)) {
    filename <- metaData(x)$fullName
  } else {
    filename <- .cleanFilename(metaData(x)$file[1])
  } 

  filename <- sub(pattern=paste("([[:punct:]]?", ending, ")?$", sep=""),
                  replacement=paste(".", ending, sep=""), x=filename,
                  ignore.case=TRUE)

  return(filename)
})

