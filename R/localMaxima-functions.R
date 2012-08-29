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
#' This method looks for local maxima in a numeric vector.
#' 
#' @param y double
#' @return logical vector of local maxima 
#' 
#' @keywords internal
#' @rdname localMaxima-functions
#' @examples
#' library("MALDIquantTools")
#' x <- c(2:4, 3:1)
#' MALDIquantTools:::.localMaxima(x)
#'

.localMaxima <- function(y) {
  ## based on a posting of Brian Ripley on r-help mailinglist
  ## https://stat.ethz.ch/pipermail/r-help/2001-January/010704.html
  windows <- embed(c(0, y, 0), 3);
  localMaxima <- max.col(windows, "first") == 2 & max.col(windows, "last") == 2
  return(localMaxima)
}

