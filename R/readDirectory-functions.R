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

#' Reads all files in one directory matching a given pattern
#'
#' Reads all files in one directory matching a given pattern.
#' WARNING: this is a recursive function!
#'
#' @param path path to directory
#' @param pattern file pattern to look for
#' @param fun function which should used to read the a single file
#' @param verbose verbose output?
#' @param ... further arguments passed to \code{fun}
#' 
#' @return list of MALDIquant::MassSpectrum objects
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum-class}}
#' @keywords internal
#' @name readDirectory
.readDirectory <- function(path, pattern, fun, verbose=FALSE, ...) {
  if (!file.exists(path)) {
    stop("Path ", sQuote(path), " doesn't exists!")
  }
  
  if (missing(pattern) && file.info(path)$isdir) {
    stop("You have to add a file ", sQuote("pattern"), "!");
  }
  
  if (missing(fun) || !is.function(fun)) {
    stop("You have to add a function ", sQuote("fun"),
         " which could read a single file!");
  } else {
    fun <- match.fun(fun)
  }
  
  if (!file.info(path)$isdir) {
    return(fun(file=path, verbose=verbose, ...));
  } else {
    if (verbose) {
      message("Look for spectra in ", sQuote(path), " ...");
    }

    ## look for files (alphabetical sort)
    files <- list.files(path=path, pattern=pattern, recursive=TRUE);

    if (length(files) <= 0) {
        stop("Directory doesn't contain any ", sQuote(pattern), " file.");
    }

    ## generate "path/files"
    files <- sapply(files, function(x) {
            x <- file.path(path, x);
            return(x);
        });

    ## read files
    data <- lapply(X=files, FUN=function(f) {
            return(fun(file=f, verbose=verbose, ...));
    });

    return(unlist(data));
  }
}

