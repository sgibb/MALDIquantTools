### Copyright 2012 Sebastian Gibb
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

#' Estimates the noise of a MassSpectrum object.
#' 
#' This method estimates the noise of mass spectrometry data
#' by Friedman's \emph{super smoother}.
#' 
#' @param x \code{double}, mass values.
#' @param y \code{double}, intensity values.
#' @param \ldots further arguments to passed to \code{\link[stats]{supsmu}}.
#'
#' @seealso 
#' \code{\link[MALDIquant]{detectPeaks}},
#' \code{\link[MALDIquant]{estimateNoise}}
#'
#' @rdname estimateNoiseSuperSmoother
#' @export
#' @examples
#' ## load library
#' library("MALDIquantTools")
#' 
#' ## load example data
#' data("fiedler2009subset", package="MALDIquant")
#'
#' ## choose only the first mass spectrum
#' s <- fiedler2009subset[[1]]
#'
#' ## transform intensities
#' s <- transformIntensity(s, sqrt)
#'
#' ## remove baseline
#' s <- removeBaseline(s)
#'
#' ## plot spectrum
#' plot(s)
#'
#' ## estimate noise
#' n <- estimateNoiseSuperSmoother(mass(s), intensity(s))
#'
#' ## draw noise on the plot
#' lines(n, col="red")
#' 
estimateNoiseSuperSmoother <- function(x, y, ...) {
  s <- supsmu(x=x, y=y, ...)
  return(cbind(x, s$y))
}

