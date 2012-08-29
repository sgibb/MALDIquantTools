\name{.localMaxima}
\alias{createAveragineReferenceTable}
\alias{.localMaxima}
\title{Find local maxima}
\usage{
  .localMaxima(y)

  createAveragineReferenceTable(averagine, minMass = 100,
    maxMass = 5000)
}
\arguments{
  \item{y}{double}

  \item{y}{double}
}
\value{
  logical vector of local maxima

  logical vector of local maxima
}
\description{
  This function looks for local maxima in a numeric vector.

  This function creates a reference table for
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}.
}
\examples{
library("MALDIquantTools")
x <- c(2:4, 3:1)
MALDIquantTools:::.localMaxima(x)
library("MALDIquantTools")
x <- c(2:4, 3:1)
MALDIquantTools:::.localMaxima(x)
}
\keyword{internal}

