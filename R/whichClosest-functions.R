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

#' Find nearest index
#' 
#' This method looks for closest index to a given value.
#' 
#' @param db double, database which contains reference values (sorted)
#' @param key double, value to look for
#' @return nearest index
#' 
#' @keywords internal
#' @rdname whichClosest-functions
#' @examples
#' library("MALDIquantTools")
#' db <- 1:10+0.2
#' MALDIquantTools:::.whichClosest(db, 2.1) # == 2
#'

.whichClosest <- function(db, key) {
  d <- abs(db-key);
  return(which.min(d))
}

