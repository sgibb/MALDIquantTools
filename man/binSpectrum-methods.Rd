\docType{methods}
\name{.binSpectrum}
\alias{.binSpectrum}
\alias{.binSpectrum,MassSpectrum-method}
\title{This method does a stupid binning of mass values.}
\arguments{
  \item{x}{a \code{\linkS4class{MassSpectrum}} object.}

  \item{nbins}{number of bins.}

  \item{from}{minimal mass (left border).}

  \item{to}{maximal mass (right border).}

  \item{fun}{aggregate function.}
}
\value{
  a binned spectrum
}
\description{
  This method does a stupid binning of mass values.
}
\note{
  This method is for internal use only. It isn't a
  sophisticated binning method.
}
\seealso{
  \code{\linkS4class{MassSpectrum}}
}
\keyword{internal}

