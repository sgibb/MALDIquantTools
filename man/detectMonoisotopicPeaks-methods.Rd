\docType{methods}
\name{detectMonoisotopicPeaks}
\alias{detectMonoisotopicPeaks}
\alias{detectMonoisotopicPeaks,list-method}
\alias{detectMonoisotopicPeaks,MassSpectrum-method}
\title{Monoisotopic Peak Detection}
\arguments{
  \item{object}{a \code{\linkS4class{MassSpectrum}} object
  or a list of \code{\linkS4class{MassSpectrum}} objects}

  \item{halfWindowSize}{\code{numeric}, half window size.
  \cr The resulting window reaches from
  \code{mass[currentIndex-halfWindowSize]} to
  \code{mass[currentIndex+halfWindowSize]}. A local maximum
  have to be the highest one in the given window to be
  recognized as peak.}

  \item{SNR}{single numeric value. \code{SNR} is an
  abbreviation for
  \emph{s}ignal-to-\emph{n}oise-\emph{r}atio. A local
  maximum has to be higher than \code{SNR*noise} to be
  recognize as peak.}

  \item{chargeState}{\emph{z}, charge states used to look
  for isotopic pattern}

  \item{isotopicDistance}{\emph{z}, distance between two
  isotopic mass (\emph{chargeState == 1})}

  \item{tolerance}{allowed difference (in Dalton) between
  measured and calculated isotopic mass}

  \item{intensityTolerance}{allowed difference between
  measured and calculated isotopic intensities}

  \item{referenceTable}{data.frame, reference table of
  isotopic pattern (default:
  \code{\link[MALDIquantTools]{averagineTable}})}

  \item{\dots}{arguments to be passed to
  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}}
}
\description{
  This method looks for monoisotopic peaks in a
  \code{\linkS4class{MassPeaks}} object. It is mostly a
  wrapper around
  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}}
  and
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}.
}
\seealso{
  \code{\linkS4class{MassPeaks}},
  \code{\linkS4class{MassSpectrum}},
  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
  \code{\link[MALDIquantTools]{averagineTable}},
  \code{\link[MALDIquantTools]{uniProtTable}}
}
\keyword{methods}

