\name{.localMaxima}
\alias{.localMaxima}
\title{Find local maxima}
\usage{
  .localMaxima(y, halfWindowSize = 1)
}
\arguments{
  \item{y}{\code{double}}

  \item{halfWindowSize}{\code{numeric}, half window size.
  \cr The resulting window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]}. A local maximum
  have to be the highest one in the given window to be
  recognized as peak.}
}
\value{
  logical vector of local maxima
}
\description{
  This function looks for local maxima in a numeric vector.
}
\examples{
library("MALDIquantTools")
x <- c(2:4, 3:1)
MALDIquantTools:::.localMaxima(x)
}
\keyword{internal}

