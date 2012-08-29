\name{.localMaxima}
\alias{.localMaxima}
\title{Find local maxima}
\usage{
  .localMaxima(y)
}
\arguments{
  \item{y}{double}
}
\value{
  logical vector of local maxima
}
\description{
  This method looks for local maxima in a numeric vector.
}
\examples{
library("MALDIquantTools")
x <- c(2:4, 3:1)
MALDIquantTools:::.localMaxima(x)
}
\keyword{internal}

