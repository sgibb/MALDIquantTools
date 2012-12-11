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
if (is.null(getGeneric("fwhm")))
  setGeneric("fwhm", function(spec, peaks) standardGeneric("fwhm"))
if (is.null(getGeneric(".fwhm")))
  setGeneric(".fwhm", function(spectrum, i) standardGeneric(".fwhm"))
if (is.null(getGeneric(".isotopicScore")))
  setGeneric(".isotopicScore", function(object, ...) standardGeneric(".isotopicScore"))
if (is.null(getGeneric("monoisotopic")))
  setGeneric("monoisotopic", function(object, ...) standardGeneric("monoisotopic"))

