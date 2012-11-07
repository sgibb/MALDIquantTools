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

.pseudoCluster <- function(x, chargeState=1, tolerance=100e-6,
                          isotopicDistance=1.00235) {

  cluster <- logical(length(x))

  ## calculate peak difference
  stepSize <- isotopicDistance/chargeState

  i <- MALDIquant:::.which.closest(x+stepSize, x)

  d <- abs(x[i]-x-stepSize)/x < tolerance
    
  cluster[d] <- TRUE
  cluster[i[d]] <- TRUE

  return(cluster)
}
