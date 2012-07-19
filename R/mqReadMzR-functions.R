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

#' Reads mass spectrometry files 
#'
#' This function reads mass spectrometry files into
#' \code{\link[MALDIquant]{MassSpectrum-class}} objects. All formats supported
#' by \code{\link[mzR]{mzR-package}} could read in.
#'
#' @param path path to directory/file
#' @param pattern file pattern to look for
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{.readMzRFile}
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum}}, 
#'  \code{\link[mzR]{mzR-package}}
#' @keywords IO
#' @export
#' @examples
#'  \dontrun{s <- mqReadMzR("/home/user/msdata/", pattern="*.mzXML")}
mqReadMzR <- function(path, pattern="*.mzML", verbose=FALSE, ...) {
  return(.readDirectory(path=path, pattern=pattern, verbose=verbose,
                        fun=.readMzRFile, ...))
}

#' Reads mass spectrometry files 
#'
#' This function reads a mass spectrometry file into a
#' \code{\link[MALDIquant]{MassSpectrum-class}} object. All formats supported
#' by \code{\link[mzR]{mzR-package}} could read in.
#'
#' @param file file path
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{\link[mzR]{openMSfile}}
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum}},
#'  \code{\link[mzR]{openMSfile}}
#' @keywords internal
#' @rdname readMzRFile

.readMzRFile <- function(file, verbose=FALSE, ...) {
  ## load mzR library
  stopifnot(require("mzR"));

  if (verbose) {
    message("Reading spectrum from ", sQuote(file), " ...")
  }
  
  if (!file.exists(file)) {
    stop("File ", sQuote(file), " doesn't exists!")
  }
  
  if (file.info(file)$isdir) {
    stop(sQuote(file), " is a directory!")
  }
  
  ## try to get absolute file path
  file <- normalizePath(file)
  
  ## load ms file
  msData <- openMSfile(filename=file, ..., verbose=verbose)
  
  ## read metadata
  metaData <- c(file=file, runInfo(msData), instrumentInfo(msData))
  
  spectra <- lapply(1:length(msData), function(x) {
      peaks <- peaks(msData, x);
      return(createMassSpectrum(mass=peaks[, 1], intensity=peaks[, 2],
                                metaData=c(metaData, header(msData, x))))
  });
  
  ## close file
  #close(msData);
  
  return(unlist(spectra));
}

