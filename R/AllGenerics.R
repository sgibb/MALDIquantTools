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

if (is.null(getGeneric(".binSpectrum")))
  setGeneric(".binSpectrum", function(x, ...) standardGeneric(".binSpectrum"))
if (is.null(getGeneric(".composeFilename")))
  setGeneric(".composeFilename", function(x, ...) standardGeneric(".composeFilename"))
if (is.null(getGeneric("detectMonoisotopicPeaks")))
  setGeneric("detectMonoisotopicPeaks", function(object, ...) standardGeneric("detectMonoisotopicPeaks"))
if (is.null(getGeneric("export")))
  setGeneric("export", function(x, ...) standardGeneric("export"))
if (is.null(getGeneric("exportCsv")))
  setGeneric("exportCsv", function(x, ...) standardGeneric("exportCsv"))
if (is.null(getGeneric("exportMsd")))
  setGeneric("exportMsd", function(x, ...) standardGeneric("exportMsd"))
if (is.null(getGeneric("fwhm")))
  setGeneric("fwhm", function(spec, peaks) standardGeneric("fwhm"))
if (is.null(getGeneric(".fwhm")))
  setGeneric(".fwhm", function(spectrum, i) standardGeneric(".fwhm"))
if (is.null(getGeneric(".isotopicScore")))
  setGeneric(".isotopicScore", function(object, ...) standardGeneric(".isotopicScore"))
if (is.null(getGeneric("monoisotopic")))
  setGeneric("monoisotopic", function(object, ...) standardGeneric("monoisotopic"))
if (is.null(getGeneric("write.csv")))
  setGeneric("write.csv", function(...) standardGeneric("write.csv"))

## make a S3 generic for write.table
#write.table <- function(x, ...) UseMethod("write.table")

## take the usual definition of write.table and set it to be the default method
#write.table.default <- utils::write.table
#setMethod(f="write.table", signature=signature(x="ANY"), function(x, ...)
#          UseMethod("write.table"))
#setMethod(f="write.table",
#  signature=signature(x="ANY", file="character",
#                      append="logical", quote="logical", sep="character",
#                      eol="character", na="character", dec="character",
#                      row.names="ANY", col.names="ANY",
#                      qmethod="character", fileEncoding="character"),
#  definition=function(x, file="", append=FALSE, quote=TRUE, sep=" ", eol="\n",
#                      na="NA", dec=".", row.names=TRUE, col.names=TRUE, 
#                      qmethod=c("escape", "double"), fileEncoding="") {
#          UseMethod("write.table")})

## allow ... argument
#formals(write.table.default) <- c(formals(write.table.default), alist(...=))

#if (is.null(getGeneric("write.table")))
#  setGeneric("write.table", function(x, file, append, quote, sep, eol, na, dec,
#                                     row.names, col.names, qmethod,
#                                     fileEncoding)
#                                     standardGeneric("write.table"))
if (is.null(getGeneric("write.table")))
  setGeneric("write.table")

