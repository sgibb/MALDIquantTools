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

#' Averagine reference table
#'
#' This dataset contains mass for combinations of averagine.
#'
#' @details
#'  To create: 
#'  \preformatted{
#'z <- read.csv(system.file("averagine.csv", package="MALDIquantTools"), stringsAsFactors=FALSE)
#'
#'averagineTable <- MALDIquantTools:::createAveragineReferenceTable(z, minMass=120, maxMass=5000)
#'  }
#'
#' @references
#'  Michael W. Senko and Steven C. Beu and Fred W. McLafferty (1995), \cr
#'  \dQuote{Determination of monoisotopic masses and ion populations for large
#'  biomolecules from resolved isotopic distributions} \cr
#'  \emph{Journal of the American Society for Mass Spectrometry}, \bold{4} (6): 229-233 \cr
#'  ISSN: 1044-0305; doi 10.1016/1044-0305(95)00017-8 \cr
#'  \url{http://www.sciencedirect.com/science/article/pii/1044030595000178}
#'  
#' @seealso \code{\link[MALDIquantTools]{uniProtTable}},
#'  \code{\link[MALDIquantTools]{createAveragineReferenceTable}}
#' @name averagineTable
#' @docType data
#' @rdname averagineTable
#' @keywords datasets
NULL
