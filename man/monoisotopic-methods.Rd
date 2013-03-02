\name{monoisotopic,MassPeaks-method}
\alias{monoisotopic}
\alias{monoisotopic,list-method}
\alias{monoisotopic,MassPeaks-method}
\title{Monoisotopic Peak Detection}
\arguments{
  \item{object}{a \code{\linkS4class{MassPeaks}} object or
  a list of \code{\linkS4class{MassPeaks}} objects}

  \item{chargeState}{\emph{z}, charge states used to look
  for isotopic pattern}

  \item{isotopicDistance}{\emph{z}, distance between two
  isotopic mass (\emph{chargeState == 1})}

  \item{tolerance}{allowed difference (in Dalton) between
  measured and calculated isotopic mass}

  \item{intensityTolerance}{allowed difference between
  measured and calculated isotopic intensities}

  \item{SNR}{\code{double}, minimal signal-to-noise ratio
  of most intense peak in an isotopic pattern.  (default:
  \code{\link[MALDIquantTools]{averagineTable}})}

  \item{referenceTable}{data.frame, reference table of
  isotopic pattern (default:
  \code{\link[MALDIquantTools]{averagineTable}})}
}
\description{
  This method looks for monoisotopic peaks in a
  \code{\linkS4class{MassPeaks}} object.
}
\seealso{
  \code{\linkS4class{MassPeaks}},
  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
  \code{\link[MALDIquantTools]{averagineTable}},
  \code{\link[MALDIquantTools]{uniProtTable}}
}
\keyword{methods}

