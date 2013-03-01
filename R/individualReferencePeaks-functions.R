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

#' This function looks for reference peaks in each individual sample.
#' In contrast to \code{\link[MALDIquant]{referencePeaks}} it returns a
#' \code{\link{list}} of \code{\linkS4class{MassPeaks}} where all non reference
#' peaks are removed and the mass are not binned. E.g. this is useful to 
#' visualise reference peaks in each individual spectrum.
#'
#' @param l list of \code{\linkS4class{MassPeaks}}
#' @param minFrequency see \code{\link[MALDIquant]{referencePeaks}}
#' @param tolerance see \code{\link[MALDIquant]{referencePeaks}}
#' @return a list of \code{\linkS4class{MassPeaks}} objects
#' @seealso \code{\link[MALDIquant]{binPeaks}}
#' @examples
#' l <- list(createMassPeaks(mass=1:5, intensity=1:5),
#'           createMassPeaks(mass=c(1, 2:4+0.2, 5.01), intensity=1:5),
#'           createMassPeaks(mass=c(1, 2:4+0.4, 5.02), intensity=1:5))
#' individualReferencePeaks(l, tolerance=0.02)
#' @export
individualReferencePeaks <- function(l, minFrequency=0.9, tolerance=0.002) {

    MALDIquant:::.stopIfNotIsMassPeaksList(l)

    ## find reference peaks by binning and filtering
    binnedPeaks <- binPeaks(l, tolerance=tolerance)

    ## calculate minimal number of peaks
    minPeakNumber <- floor(minFrequency * length(binnedPeaks))

    ## fetch mass
    mass <- sort(unique(MALDIquant:::.unlist(lapply(binnedPeaks, function(x)x@mass))),
                 method="quick")

    ## generate peak matrix
    pm <- intensityMatrix(binnedPeaks)
    exclude <- MALDIquant:::.unlist(apply(pm, 2, 
                 function(x)(sum(!is.na(x)) < minPeakNumber)))
    exclude <- mass[exclude]

    ## search peak position
    l <- mapply(FUN=function(pre, post) {
      e <- post@mass %in% exclude;
      pre@mass <- pre@mass[!e];
      pre@intensity <- pre@intensity[!e];
      pre@snr <- pre@snr[!e];
      return(pre)
    }, pre=l, post=binnedPeaks);

    return(l);
}
