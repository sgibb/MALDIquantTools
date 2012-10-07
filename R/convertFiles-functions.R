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

#' This function converts mass spectra stored in fid/mzXML files into
#' ASCII/CSV/MSD files.
#'
#' @param path path to files
#' @param from source file format
#' @param to target file format
#'
#' @export
#'
#' @rdname convertFiles
convertFiles <- function(path, from=c("fid", "mzXML"),
                         to=c("ascii", "csv", "msd")) {
  from <- match.arg(from, c("fid", "mzXML"), several.ok=FALSE)
  to <- match.arg(to, c("ascii", "csv", "msd"), several.ok=FALSE)

  s <- switch(from,
    "fid"={
      stopifnot(require("readBrukerFlexData"))
      mqReadBrukerFlex(path, verbose=TRUE)
    },
    "mzXML"={
      stopifnot(require("readMzXmlData"))
      mqReadMzXml(path, verbose=TRUE)
    },
    stop(sQuote(from), " is no suitable input format!"))

  switch(to,
    "ascii"={export(s, path)},
    "csv"={exportCsv(s, path)},
    "msd"={exportMsd(s, path)},
    stop(sQuote(from), " is no suitable output format!"))

  invisible(NULL)
}
