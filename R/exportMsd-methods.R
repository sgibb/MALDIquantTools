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
#' This methods prints a \code{\linkS4class{MassSpectrum}} object 
#' to a file or connection in mMass' MSD format.
#' 
#' @param x \code{\linkS4class{MassSpectrum}} object
#' @param file file name
#' @param peaks optional,\code{\linkS4class{MassPeaks}} object
#' @param path directory path
#' @param force create \code{path} if not exists
#'
#' @seealso \code{\linkS4class{MassSpectrum}}
#' @references \url{http://www.mmass.org}
#' @aliases exportMsd,MassSpectrum-method exportMsd,list-method
#' @docType methods
#' @keywords methods
#' @rdname exprtMsd-methods
#' @exportMethod exportMsd
#' @examples
#' \dontrun{
#' s <- createMassSpectrum(mass=1:5, intensity=6:10,
#'                         metaData=list(fullName="sample1")
#' exportMsd(s, "/home/user/msdata/test.msd")
#' }
setMethod(f="exportMsd",
  signature=signature(x="MassSpectrum"),
  definition=function(x, file, peaks) {
  return(.writeMsdDocument(x=x, file=file, peaks=peaks))
})

setMethod(f="exportMsd",
  signature=signature(x="list"),
  definition=function(x, path, peakList, force=TRUE) {

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

  if (!missing(peaks)) {
    stopifnot(length(x) == length(peaks))
  }

  for (i in seq(along=x)) {
    spec <- x[[i]]
    file <- file.path(path, .composeFilename(spec, ending="msd"))
    if (!missing(peaks)) {
      exportMsd(spec, file, peaks[[i]])
    } else {
      exportMsd(spec, file)
    }
  }

  invisible()
})

