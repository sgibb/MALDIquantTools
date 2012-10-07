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

#' Find local maxima
#' 
#' This function looks for local maxima in a numeric vector.
#' 
#' @param y \code{double}
#' @param halfWindowSize \code{numeric}, half window size. \cr
#'  The resulting window reaches from \code{mass[currentIndex-halfWindowSize]}
#'  to \code{mass[currentIndex+halfWindowSize]}. A local maximum have to be
#'  the highest one in the given window to be recognized as peak.
#' @return logical vector of local maxima 
#' 
#' @export
#' @rdname localMaxima-functions
#' @examples
#' library("MALDIquantTools")
#' x <- c(2:4, 3:1)
#' MALDIquantTools:::.localMaxima(x)
#'

estimateNoiseLocalMad <- function(mass, intensity, halfWindowSize=20) {
  windowSize <- 2 * halfWindowSize + 1
  windows <- embed(c(rep(NA, halfWindowSize), intensity, 
                     rep(NA, halfWindowSize)), 
                   windowSize)
  return(cbind(mass, apply(windows, 1, mad, na.rm=TRUE)))
}

