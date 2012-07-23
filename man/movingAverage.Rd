\name{movingAverage}
\alias{movingAverage}
\title{This function runs a simple 2-side moving average.}
\usage{
  movingAverage(y, halfWindowSize)
}
\arguments{
  \item{y}{intensity values}

  \item{halfWindowSize}{half window size. The resulting
  window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]}.}
}
\value{
  a smooth intensity vector
}
\description{
  This function runs a simple 2-side moving average.
}
\examples{
movingAverage(1:10, 2)
# NA NA  3  4  5  6  7  8 NA NA
}
\seealso{
  \code{\link{filter}}
}

