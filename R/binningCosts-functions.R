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

#' This function calculates binning costs (differences between masses pre and
#' post binning).
#'
#' @param pre list of \code{\linkS4class{MassPeaks}} (pre binning)
#' @param post list of \code{\linkS4class{MassPeaks}} (post binning)
#' @param relative logical, should costs represent by a costs vs mass position
#'  ratio?
#' @return a list of binning costs (percental differences)
#' @seealso \code{\link[MALDIquant]{binPeaks}}
#' @examples
#' pre <- list(createMassPeaks(mass=1:5, intensity=1:5),
#'             createMassPeaks(mass=1:5+0.1, intensity=1:5),
#'             createMassPeaks(mass=1:5+0.2, intensity=1:5))
#' post <- binPeaks(pre)
#'
#' binningCosts(pre, post)
#' # NA NA  3  4  5  6  7  8 NA NA
#' @export
binningCosts <- function(pre, post, relative=TRUE) {
  MALDIquant:::.stopIfNotMassPeaksList(pre)
  MALDIquant:::.stopIfNotMassPeaksList(post)
  stopifnot(length(pre) == length(post))

  costsList <- mapply(FUN=function(pre, post, relative) {
    costs <- abs(pre@mass-post@mass)
    
    if (relative) {
      costs <- costs/post@mass
    }

    return(costs)
  }, pre=pre, post=post, relative=relative, SIMPLIFY=FALSE)

  names(costsList) <- names(post)

  return(costsList)
}
