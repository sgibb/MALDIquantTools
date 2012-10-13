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
  definition=function(object, chargeState=1:2, 
                      isotopicDistance=1.004,
                      # 1.004 M. Wehofsky et al., Eur. J. Mass Spectrom. 7, 39–46 (2001)
                      tolerance=0.1, intensityTolerance=0.2,
                      referenceTable) {

  if (missing(referenceTable)) {
    data("averagineTable")
    referenceTable <- get("averagineTable", env=globalenv())
  }

  ## start with highest charge state
  chargeState <- sort(chargeState, decreasing=TRUE)

  ## calculate mass difference
  dMass <- c(diff(object@mass), Inf)

  monoisotopic <- logical(length(object))

  localMaxima <- .localMaxima(object@intensity)

  for (z in chargeState) {
    ## calculate peak difference
    stepSize <- isotopicDistance/z
    d <- dMass-isotopicDistance

    ## isotopic distance?
    isotopic <- abs(d) <= tolerance/z

    ## is highest peak in an isotopic pattern?
    apexIdx <- which(localMaxima & isotopic)
    apexMass <- object@mass[apexIdx]

    ## find closest reference setup
    closest <- unlist(lapply((apexMass*z), .whichClosest,
                              db=referenceTable$monoisotopicMass))

    ## steps to go left (backwards)
    steps <- (referenceTable$apexIdx[closest]-1)/z

    ## relative apex intensity apex/mono
    relApexIntensity <- referenceTable$relativeIntensityApexToMonoisotopic[closest]
    
    ### find potential monotopic positions
    ### sometimes (between step changes (0 to 1, 1 to 2, ...)) the steps are one
    ### dalton to short
    potMonoIdx <- unlist(lapply(apexMass-(c(steps+1, steps, steps-1)*stepSize),
                                .whichClosest, db=object@mass))
    ### avoid "out of boundaries" error
    potMonoIdx[potMonoIdx == 0] <- 1

    potMonoMass <- object@mass[potMonoIdx]
    potRelIntensity <- object@intensity[apexIdx]/object@intensity[potMonoIdx]

    ## is mass distance correct?
    d <- apexMass-potMonoMass-(abs(c(steps+1, steps, steps-1))*stepSize)
    bMonoMass <- abs(d) <= tolerance/z

    ## is intensity correct?
    dIntensity <- abs(potRelIntensity-relApexIntensity)/potRelIntensity 
    bMonoIntensity <- dIntensity <= intensityTolerance
    
    ## are mass and intensity correct?
    bPotMono <- matrix(bMonoMass & bMonoIntensity, nrow=3, byrow=TRUE)

    if (length(bPotMono)) {
      ## find correct indentified monoisotopic peaks
      na <- !bPotMono
      bPotMono[na] <- 0
      bPotMono[!na] <- 1
      bPotMono <- t(bPotMono)

      ## prefer lowest mass
      maxCol <- max.col(bPotMono, "first")
      isMono <- matrix(FALSE, nrow=ncol(bPotMono), ncol=nrow(bPotMono))
      
      for (i in seq(along=maxCol)) {
        isMono[maxCol[i], i] <- TRUE
      }

      isMono[na] <- FALSE

      monoIdx <- potMonoIdx[t(isMono)]
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

