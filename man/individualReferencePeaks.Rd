\name{individualReferencePeaks}
\alias{individualReferencePeaks}
\title{This function looks for reference peaks in each individual sample.
In contrast to \code{\link[MALDIquant]{referencePeaks}} it returns a
\code{\link{list}} of \code{\linkS4class{MassPeaks}} where all non reference
peaks are removed and the mass are not binned. This is useful to visualize
reference peaks in each individual spectrum.}
\usage{
  individualReferencePeaks(l, minFrequency = 0.9,
    tolerance = 0.002)
}
\arguments{
  \item{l}{list of \code{\linkS4class{MassPeaks}}}

  \item{minFrequency}{see
  \code{\link[MALDIquant]{referencePeaks}}}

  \item{tolerance}{see
  \code{\link[MALDIquant]{referencePeaks}}}
}
\value{
  a list of \code{\linkS4class{MassPeaks}} objects
}
\description{
  This function looks for reference peaks in each
  individual sample. In contrast to
  \code{\link[MALDIquant]{referencePeaks}} it returns a
  \code{\link{list}} of \code{\linkS4class{MassPeaks}}
  where all non reference peaks are removed and the mass
  are not binned. This is useful to visualize reference
  peaks in each individual spectrum.
}
\examples{
l <- list(createMassPeaks(mass=1:5, intensity=1:5),
          createMassPeaks(mass=c(1, 2:4+0.2, 5.01), intensity=1:5),
          createMassPeaks(mass=c(1, 2:4+0.4, 5.02), intensity=1:5))
individualReferencePeaks(l, tolerance=0.02)
}
\seealso{
  \code{\link[MALDIquant]{binPeaks}}
}

