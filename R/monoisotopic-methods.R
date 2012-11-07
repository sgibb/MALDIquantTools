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
#' object.
#' 
#' @param object a \code{\linkS4class{MassPeaks}} object or a list of
#'  \code{\linkS4class{MassPeaks}} objects
#' @param chargeState \emph{z}, charge states used to look for isotopic pattern
#' @param isotopicDistance \emph{z}, distance between two isotopic mass
#' (\emph{chargeState == 1})
#' @param tolerance allowed difference (in Dalton) between measured and
#'  calculated isotopic mass
#' @param intensityTolerance allowed difference between measured and
#'  calculated isotopic intensities
#' @param referenceTable data.frame, reference table of isotopic pattern
#'  (default: \code{\link[MALDIquantTools]{averagineTable}})
#'
#' @seealso \code{\linkS4class{MassPeaks}},
#'  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
#'  \code{\link[MALDIquantTools]{averagineTable}},
#'  \code{\link[MALDIquantTools]{uniProtTable}}
#' @aliases monoisotopic,MassPeaks-method monoisotopic,list-method
#' @docType methods
#' @keywords methods
#' @rdname monoisotopic-methods
#' @exportMethod monoisotopic
#' @examples
#' p <- createMassPeaks(mass=c(1:5, 9, 20:24),
#'                      intensity=c(5:1, 1, 3, 5:2))
#'
#' referenceTable <- data.frame(monoisotopicMass=c(1.01, 20.2),
#'                              relativeIntensityApexToMonoisotopic=c(1, 5/3),
#'                              apexIdx=c(1, 2))
#' m <- monoisotopic(p, chargeState=1)
#' mass(m) # c(1, 20)
#'

## MassPeaks
setMethod(f="monoisotopic",
  signature=signature(object="MassPeaks"),
  definition=function(object, chargeState=1, 
                      isotopicDistance=1.00235,
                      ## Park et al 2008, Anal. Chem.
                      tolerance=1e-3, intensityTolerance=0.2,
                      SNR=quantile(snr(object), probs=0.8),
                      referenceTable) {

  if (missing(referenceTable)) {
    data("averagineTable")
    referenceTable <- get("averagineTable", env=globalenv())
  }

  ## start with highest charge state
  chargeState <- sort(chargeState, decreasing=TRUE)

  monoisotopic <- logical(length(object))

  localMaxima <- MALDIquant:::.localMaxima(object@intensity, halfWindowSize=1)

  for (z in chargeState) {
    ## calculate peak difference
    stepSize <- isotopicDistance/z

    ## isotopic distance?
    isotopic <- .pseudoCluster(object@mass, chargeState=z,
                               isotopicDistance=isotopicDistance,
                               tolerance=tolerance)

    ## is highest peak in an isotopic pattern?
    apexIdx <- which(localMaxima & isotopic)
    apexIdx <- apexIdx[object@snr[apexIdx] >= SNR]
    apexMass <- object@mass[apexIdx]

    ## find closest reference setup
    closest <- MALDIquant:::.which.closest(apexMass*z,
                                           referenceTable$apexMass)

    ## steps to go left (backwards)
    steps <- (referenceTable$apexIdx[closest]-1)/z

    ### find potential monotopic positions
    ### sometimes (between step changes (0 to 1, 1 to 2, ...)) the steps are one
    ### dalton to short
    potMonoIdx <- MALDIquant:::.which.closest(
      apexMass-(c(steps+1, steps, steps-1)*stepSize), object@mass)

    ### avoid "out of boundaries" error
    potMonoIdx[potMonoIdx == 0] <- 1

    potMonoMass <- object@mass[potMonoIdx]

    ## is mass distance correct?
    nxt <- MALDIquant:::.which.closest(potMonoMass+stepSize, object@mass)
    d <- object@mass[nxt]-(potMonoMass+stepSize)
    bMonoMass <- abs(d)/potMonoMass < tolerance

    ## is intensity correct?
    dIntensity <- .isotopicScore(object=object, monoIdx=potMonoIdx,
                                 apexIdx=apexIdx, stepSize=stepSize,
                                 referenceTable=referenceTable, closest=closest,
                                 intensityTolerance=intensityTolerance)
    ## score valid? (>0)
    bMonoIntensity <- dIntensity > 0

    ## are mass and intensity correct?
    bPotMono <- matrix(bMonoMass & bMonoIntensity, ncol=3, byrow=FALSE)

    if (length(bPotMono)) {
      ## find correct indentified monoisotopic peaks
      na <- !bPotMono
      bPotMono[na] <- 0
      bPotMono[!na] <- 1
      dIntensity[na] <- -Inf

      ## prefer highest scored intensity to select monoisotopic mass
      maxCol <- max.col(dIntensity, "first")
      
      isMono <- matrix(FALSE, nrow=nrow(bPotMono), ncol=ncol(bPotMono))
      
      for (i in seq(along=maxCol)) {
        isMono[i, maxCol[i]] <- TRUE
      }

      isMono[na] <- FALSE

      monoIdx <- potMonoIdx[isMono]
      monoisotopic[monoIdx] <- TRUE
    }
  }

  object@mass <- object@mass[monoisotopic]
  object@intensity <- object@intensity[monoisotopic]
  object@snr <- object@snr[monoisotopic]

  return(object)
})

## list
setMethod(f="monoisotopic",
  signature=signature(object="list"),
  definition=function(object, ...) {
  ## test arguments
  MALDIquant:::.stopIfNotMassPeaksList(object)
  
  return(lapply(object, monoisotopic, ...))
})

