\name{.isotopicScore}
\alias{.isotopicScore}
\title{Calculate isotopic score.}
\arguments{
  \item{object}{\code{\linkS4class{MassPeaks}} object.}

  \item{monoIdx}{\code{numeric}, index of potential
  monoisotopic mass.}

  \item{apexIdx}{\code{numeric}, index of apex in
  pseudocluster.}

  \item{stepSize}{\code{double}, calculated difference
  between isotopic mass values.}

  \item{referenceTable}{data.frame, reference table of
  isotopic pattern (default:
  \code{\link[MALDIquantTools]{averagineTable}})}

  \item{closest}{\code{numeric}, index of closest mass in
  \code{referenceTable}.}

  \item{intensityTolerance}{\code{double}, allowed
  difference between measured and calculated isotopic
  intensities.}
}
\value{
  score \code{matrix} (row1: theoretical mono-1Da, row2:
  mono, row3: mono+1Da)
}
\description{
  This function calculates a score for a isotopic
  pseudocluster in monoisotopic peak detection. Inspired by
  Park et al 2008.
}
\references{
  Park, K. and Yoon, J. Y. and Lee, S. and Paek, E. and
  Park, H. and Jung, H. J. and Lee, S. W. (2008),
  \dQuote{Isotopic peak intensity ratio based algorithm for
  determination of isotopic clusters and monoisotopic
  masses of polypeptides from high-resolution mass
  spectrometric data.} \emph{Analytical Chemistry},
  \bold{80}: 7294-7303. \cr
}
\seealso{
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}
  \code{\link[MALDIquantTools]{.pseudoCluster}}
}
\keyword{internal}

