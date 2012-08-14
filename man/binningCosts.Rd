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
post <- binPeaks(pre, tolerance=0.2)

str(binningCosts(pre, post))
# List of 3
# $ : num [1:5] 0.0909 0.0476 0.0323 0.0244 0.0196
# $ : num [1:5] 0 0 0 0 0
# $ : num [1:5] 0.0909 0.0476 0.0323 0.0244 0.0196
}
\seealso{
  \code{\link[MALDIquant]{binPeaks}}
}

