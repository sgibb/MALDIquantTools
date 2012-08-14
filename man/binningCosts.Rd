\name{binningCosts}
\alias{binningCosts}
\title{This function calculates binning costs (differences between masses pre and
post binning).}
\usage{
  binningCosts(pre, post, relative = TRUE)
}
\arguments{
  \item{pre}{list of \code{\linkS4class{MassPeaks}} (pre
  binning)}

  \item{post}{list of \code{\linkS4class{MassPeaks}} (post
  binning)}

  \item{relative}{logical, should costs represent by a
  costs vs mass position ratio?}
}
\value{
  a list of binning costs (percental differences)
}
\description{
  This function calculates binning costs (differences
  between masses pre and post binning).
}
\examples{
pre <- list(createMassPeaks(mass=1:5, intensity=1:5),
            createMassPeaks(mass=1:5+0.1, intensity=1:5),
            createMassPeaks(mass=1:5+0.2, intensity=1:5))
post <- binPeaks(pre)

binningCosts(pre, post)
# NA NA  3  4  5  6  7  8 NA NA
}
\seealso{
  \code{\link[MALDIquant]{binPeaks}}
}

