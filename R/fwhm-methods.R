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

#' FWHM
#'
#' This method calculates the \emph{f}ull-\emph{w}idth at
#' \emph{h}alf-\emph{m}aximum for each peak.
#'
#' @param spec a \code{\linkS4class{MassSpectrum}} object or a list of
#'  \code{\linkS4class{MassSpectrum}} objects
#' @param peaks a \code{\linkS4class{MassPeaks}} object or a list of
#'  \code{\linkS4class{MassPeaks}} objects
#'
#' @seealso \code{\linkS4class{MassPeaks}}, \code{\linkS4class{MassSpectrum}}
#' @aliases fwhm fwhm,MassSpectrum,MassPeaks-method fwhm,list,list-method
#' @docType methods
#' @keywords methods
#' @rdname fwhm-methods
#' @exportMethod fwhm
#'
setMethod(f="fwhm",
  signature=signature(spec="MassSpectrum", peaks="MassPeaks"),
  definition=function(spec, peaks) {

  ## empty?
  if (MALDIquant:::.isEmptyWarning(spec) ||
      MALDIquant:::.isEmptyWarning(peaks)) {
    return(NA)
  }

  idx <- which(spec@mass %in% peaks@mass)
  fwhm <- unlist(lapply(idx, .fwhm, spectrum=spec))
  return(fwhm)
})

## list
setMethod(f="fwhm",
  signature=signature(spec="list", peaks="list"),
  definition=function(spec, peaks) {

  ## test arguments
  MALDIquant:::.stopIfNotIsMassSpectrumList(spec);
  MALDIquant:::.stopIfNotIsMassPeaksList(peaks);

  stopifnot(length(spec) == length(peaks))

  return(lapply(1:length(spec), function(i)fwhm(spec[[i]], peaks[[i]])))
})

## work horse function
setMethod(f=".fwhm",
    signature=signature(spectrum="MassSpectrum", i="numeric"),
    definition=function(spectrum, i) {
  n <- length(spectrum)
  left <- ifelse(i < 1, 1, i)
  right <- ifelse(i > n, n, i)

  hm <- spectrum@intensity[i]/2

  while (left > 1 && spectrum@intensity[left] > hm) {
    left <- left-1
  }
  while (right < n && spectrum@intensity[right] > hm) {
    right <- right+1
  }

  ## interpolate x values
  xleft <- approx(x=spectrum@intensity[left:(left+1)],
                  y=spectrum@mass[left:(left+1)], xout=hm)$y
  xright <- approx(x=spectrum@intensity[(right-1):right],
                   y=spectrum@mass[(right-1):right], xout=hm)$y

  return(abs(xleft-xright))
})

