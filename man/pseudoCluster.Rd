\name{.pseudoCluster}
\alias{.pseudoCluster}
\title{Find pseudoclusters.}
\usage{
  .pseudoCluster(x, chargeState = 1, tolerance = 1e-04,
    isotopicDistance = 1.00235)
}
\arguments{
  \item{x}{\code{double}, vector of mass values}

  \item{chargeState}{\code{double}, calculate
  pseudoclusters for \code{chargeState} charged ions.}

  \item{tolerance}{\code{double}, allowed deviation.}

  \item{isotopicDistance}{\code{double}, average distance
  between isotopic mass values.}
}
\value{
  \code{logical} vector
}
\description{
  This function looks for (pseudo)isotopic clusters needed
  in monoisotopic peak detection.
}
\seealso{
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}
}
\keyword{internal}

