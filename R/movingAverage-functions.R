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

#' This function runs a simple 2-side moving average.
#'
#' @param y intensity values
#' @param halfWindowSize half window size. The resulting window reaches from
#'  \code{mass[currentIndex-halfWindowSize]} to
#'  \code{mass[currentIndex+halfWindowSize]}.
#' @return a smooth intensity vector
#' @seealso \code{\link{filter}}
#' @examples
#' movingAverage(1:10, 2)
#' # NA NA  3  4  5  6  7  8 NA NA
#' @export
movingAverage <- function(y, halfWindowSize) {
  windowSize <- halfWindowSize*2+1
  y <- filter(x=y, filter=rep(1, windowSize)/windowSize, sides=2)
  attributes(y) <- NULL
  return(y)
}
