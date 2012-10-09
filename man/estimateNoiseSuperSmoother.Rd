\name{estimateNoiseSuperSmoother}
\alias{estimateNoiseSuperSmoother}
\title{Estimates the noise of a MassSpectrum object.}
\usage{
  estimateNoiseSuperSmoother(x, y, ...)
}
\arguments{
  \item{x}{\code{double}, mass values.}

  \item{y}{\code{double}, intensity values.}

  \item{\ldots}{further arguments to passed to
  \code{\link[stats]{supsmu}}.}
}
\description{
  This method estimates the noise of mass spectrometry data
  by Friedman's \emph{super smoother}.
}
\examples{
## load library
library("MALDIquantTools")

## load example data
data("fiedler2009subset", package="MALDIquant")

## choose only the first mass spectrum
s <- fiedler2009subset[[1]]

## transform intensities
s <- transformIntensity(s, sqrt)

## remove baseline
s <- removeBaseline(s)

## plot spectrum
plot(s)

## estimate noise
n <- estimateNoiseSuperSmoother(mass(s), intensity(s))

## draw noise on the plot
lines(n, col="red")
}
\seealso{
  \code{\link[MALDIquant]{detectPeaks}},
  \code{\link[MALDIquant]{estimateNoise}}
}

