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

#' Monoisotopic Peak Detection
#' 
#' This method looks for monoisotopic peaks in a \code{\linkS4class{MassPeaks}}
#' object. It is mostly a wrapper around
#' \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}} and
#' \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}.
#' 
#' @param object a \code{\linkS4class{MassSpectrum}} object or a list of
#'  \code{\linkS4class{MassSpectrum}} objects
#' @param halfWindowSize \code{numeric}, half window size. \cr
#'  The resulting window reaches from \code{mass[currentIndex-halfWindowSize]}
#'  to \code{mass[currentIndex+halfWindowSize]}. A local maximum have to be
#'  the highest one in the given window to be recognized as peak.
#' @param SNR single numeric value. \code{SNR} is an abbreviation for
#'  \emph{s}ignal-to-\emph{n}oise-\emph{r}atio. A local maximum has to 
#'  be higher than \code{SNR*noise} to be recognize as peak.
#' @param chargeState \emph{z}, charge states used to look for isotopic pattern
#' @param isotopicDistance \emph{z}, distance between two isotopic mass
#' (\emph{chargeState == 1})
#' @param tolerance allowed difference (in Dalton) between measured and
#'  calculated isotopic mass
#' @param intensityTolerance allowed difference between measured and
#'  calculated isotopic intensities
#' @param referenceTable data.frame, reference table of isotopic pattern
#'  (default: \code{\link[MALDIquantTools]{averagineTable}})
#' @param \dots arguments to be passed to
#'  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}
#'
#' @seealso \code{\linkS4class{MassPeaks}}, \code{\linkS4class{MassSpectrum}},
#'  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
#'  \code{\link[MALDIquantTools]{averagineTable}},
#'  \code{\link[MALDIquantTools]{uniProtTable}}
#' @aliases detectMonoisotopicPeaks,MassSpectrum-method
#'  detectMonoisotopicPeaks,list-method
#' @docType methods
#' @keywords methods
#' @rdname detectMonoisotopicPeaks-methods
#' @exportMethod detectMonoisotopicPeaks
#'

## MassSpectrum
setMethod(f="detectMonoisotopicPeaks",
  signature=signature(object="MassSpectrum"),
  definition=function(object, 
                      halfWindowSize=20, SNR=2,
                      chargeState=1:2, 
                      isotopicDistance=1.004,
                      # 1.004 M. Wehofsky et al., Eur. J. Mass Spectrom. 7, 39–46 (2001)
                      tolerance=0.1, intensityTolerance=0.2,
                      referenceTable, ...) {

  object <- detectPeaks(object, halfWindowSize=halfWindowSize, SNR=0, ...)
  object <- monoisotopic(object, chargeState=chargeState,
                         isotopicDistance=isotopicDistance, tolerance=tolerance,
                         intensityTolerance=intensityTolerance,
                         referenceTable=referenceTable)

  include <- object@snr > SNR

  object@mass <- object@mass[include]
  object@intensity <- object@intensity[include]
  object@snr <- object@snr[include]

  return(object)
})

## list
setMethod(f="detectMonoisotopicPeaks",
  signature=signature(object="list"),
  definition=function(object, ...) {
  ## test arguments
  MALDIquant:::.stopIfNotMassSpectrumList(object)
  
  return(lapply(object, detectMonoisotopicPeaks, ...))
})

