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

#' This method does a stupid binning of mass values.
#' 
#' @note This method is for internal use only. It isn't a sophisticated binning
#' method.
#'
#' @param x a \code{\linkS4class{MassSpectrum}} object.
#' @param nbins number of bins.
#' @param from minimal mass (left border).
#' @param to maximal mass (right border).
#' @param fun aggregate function.
#' @return a binned spectrum
#'
#' @seealso \code{\linkS4class{MassSpectrum}}
#' @keywords internal
#' @aliases .binSpectrum,MassSpectrum-method
#' @docType methods
#' @rdname binSpectrum-methods
#'
setMethod(f=".binSpectrum",
  signature=signature(x="MassSpectrum"),
  definition=function(x, nbins, from=min(x@intensity), to=max(x@intensity), fun=max) {

  fun <- match.fun(fun)

  bins <- seq(from=from, to=to, length.out=nbins+1)
  cuts <- cut(x@mass, bins, include.lowest=TRUE);

  x@mass <- (head(bins, -1)+bins[-1])/2
  x@intensity <- unname(sapply(split(x@intensity, cuts), fun));

  return(x)
})

## list
setMethod(f=".binSpectrum",
  signature=signature(x="list"),
  definition=function(x, nbins, from, to, fun=max) {

  MALDIquant:::.stopIfNotIsMassSpectrumList(x)

  if (missing(from)) {
    from <- min(unlist(sapply(x, mass)))
  }
  if (missing(to)) {
    to <- max(unlist(sapply(x, mass)))
  }
  
  return(lapply(x, .binSpectrum, nbins=nbins, from=from, to=to, fun=fun))
})
