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
#' This function reads mass spectrometry data stored in ASCII text files into
#' \code{\link[MALDIquant]{MassSpectrum-class}} objects. \cr
#' Files have to have two columns (1st: mass value, 2nd: intensity value).
#'
#' @note \code{mqReadMzAscii} and \code{mqReadMzCsv} use the same internal
#'  method but with different defaults.
#'
#' @param path path to directory/file
#' @param pattern file pattern to look for
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{.readMzAsciiFile}
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum}},
#' \code{\link{mqReadMzCsv}}
#' @keywords IO
#' @export
#' @examples
#'  \dontrun{s <- mqReadMzAscii("/home/user/msdata/")}
mqReadMzAscii <- function(path, pattern="*.txt", verbose=FALSE, ...) {
  return(.readDirectory(path=path, pattern=pattern, verbose=verbose,
                        fun=.readMzAsciiFile, ...))
}

#' Reads mass spectrometry files
#'
#' This function reads mass spectrometry data stored in CSV files into
#' \code{\link[MALDIquant]{MassSpectrum-class}} objects. \cr
#' Files have to have two columns (1st: mass value, 2nd: intensity value).
#'
#' @note \code{mqReadMzAscii} and \code{mqReadMzCsv} use the same internal
#'  method but with different defaults.
#'
#' @param path path to directory/file
#' @param pattern file pattern to look for
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{.readMzAsciiFile}
#' @param sep the field separator character (see also \code{\link{read.table}})
#' @param header a logical value indicating whether the file contains the
#'  names of the variables as its first line (see also \code{\link{read.table}})
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum}},
#' \code{\link{mqReadMzAscii}}
#' @keywords IO
#' @export
#' @examples
#'  \dontrun{s <- mqReadMzCsv("/home/user/msdata/")}
mqReadMzCsv <- function(path, pattern="*.csv", verbose=FALSE, 
                          sep=",", header=TRUE, ...) {
  return(.readDirectory(path=path, pattern=pattern, verbose=verbose,
                        fun=.readMzAsciiFile, sep=sep, header=header, ...))
}

#' Reads mass spectrometry files 
#'
#' This functions reads mass spectrometry data stored in ASCII text files into
#' \code{\link[MALDIquant]{MassSpectrum-class}} object. 
#'
#' @param file file path
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{\link{read.table}}
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum}},
#'  \code{\link{read.table}}
#' @keywords internal
#' @rdname readMzAsciiFile
.readMzAsciiFile <- function(file, verbose=FALSE, ...) {
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
  peaks <- read.table(file=file, ...)
  
  return(list(createMassSpectrum(mass=peaks[, 1], intensity=peaks[, 2],
                                 metaData=list(file=file))))
}

