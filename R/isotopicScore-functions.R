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

#' Calculate isotopic score.
#'
#' This function calculates a score for a isotopic pseudocluster in monoisotopic
#' peak detection. Inspired by Park et al 2008.
#'
#' @param object \code{\linkS4class{MassPeaks}} object.
#' @param monoIdx \code{numeric}, index of potential monoisotopic mass.
#' @param apexIdx \code{numeric}, index of apex in pseudocluster.
#' @param stepSize \code{double}, calculated difference between isotopic mass
#'  values.
#' @param referenceTable data.frame, reference table of isotopic pattern
#'  (default: \code{\link[MALDIquantTools]{averagineTable}})
#' @param closest \code{numeric}, index of closest mass in
#'  \code{referenceTable}.
#' @param intensityTolerance allowed difference between measured and
#'  calculated isotopic intensities.
#' 
#' @return score \code{matrix} 
#'  (row1: theoretical mono-1Da, row2: mono, row3: mono+1Da)
#'
#' @references
#'  
#' Park, K. and Yoon, J. Y. and Lee, S. and Paek, E. and Park, H. and Jung, H. J. 
#' and Lee, S. W. (2008),
#' \dQuote{Isotopic peak intensity ratio based algorithm for determination of 
#'  isotopic clusters and monoisotopic masses of polypeptides from high-resolution
#'  mass spectrometric data.} 
#'  \emph{Analytical Chemistry}, \bold{80}: 7294-7303. \cr
#'
#' @seealso \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}
#'  \code{\link[MALDIquantTools]{.pseudoCluster}}
#' @keywords internal
#' @name isotopicScore

## MassPeaks
setMethod(f=".isotopicScore",
  signature=signature(object="MassPeaks"),
  definition=function(object, monoIdx, apexIdx, stepSize,
                      referenceTable, closest, intensityTolerance) {
 
  prv <- MALDIquant:::.which.closest(object@mass[monoIdx]-stepSize, object@mass)
  nxt <- MALDIquant:::.which.closest(object@mass[monoIdx]+stepSize, object@mass)
  nnxt <- MALDIquant:::.which.closest(object@mass[monoIdx]+2*stepSize,
                                      object@mass)

  ## calculate observed ratios
  iMiA <- object@intensity[monoIdx]/object@intensity[apexIdx]
  i1i0 <- object@intensity[nxt]/object@intensity[monoIdx]
  i2i1 <- object@intensity[nnxt]/object@intensity[nxt]

  ## calculate differences in ratios
  ma <- (1-abs(iMiA-referenceTable$MvsA[closest])/
         (intensityTolerance*referenceTable$MvsA[closest]))
  r1 <- (1-abs(i1i0-referenceTable$I1vsI0[closest])/
         (intensityTolerance*referenceTable$I1vsI0[closest]))
  r2 <- (1-abs(i2i1-referenceTable$I2vsI1[closest])/
         (intensityTolerance*referenceTable$I2vsI1[closest]))

  isFirstPeakMono <- object@mass[monoIdx] < 1750

  ## calculate score
  s <- ifelse(isFirstPeakMono, ma, ma + r1 + r2)

  ## remove some false positives
  isNextPeakLarger <- object@intensity[nxt] > object@intensity[monoIdx]
  isPrevPeakTooLarge <- object@intensity[prv] > object@intensity[nnxt] |
                        object@intensity[prv] > object@intensity[monoIdx] |
                        object@intensity[prv]/object@intensity[monoIdx] >
                        referenceTable$I1vsI0[closest]

  s <- ifelse((isFirstPeakMono & isNextPeakLarger) |
              isPrevPeakTooLarge, -Inf, s)
  
  ## create matrix (row1: theoretical mono-1Da, row2: mono, row3: mono+1Da)
  sMatrix <- matrix(s, ncol=3, nrow=length(closest), byrow=FALSE) 

  return(sMatrix)
})
