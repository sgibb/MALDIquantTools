\docType{methods}
\name{detectMonoisotopicPeaks}
\alias{detectMonoisotopicPeaks}
\alias{detectMonoisotopicPeaks,list-method}
\alias{detectMonoisotopicPeaks,MassSpectrum-method}
\title{Monoisotopic Peak Detection}
\arguments{
  \item{object}{a \code{\linkS4class{MassSpectrum}} object
  or a list of \code{\linkS4class{MassSpectrum}} objects}

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
}
\description{
  This method looks for monoisotopic peaks in a
  \code{\linkS4class{MassPeaks}} object.
}
\examples{
p <- createMassPeaks(mass=c(1:5, 9, 20:24),
                     intensity=c(5:1, 1, 3, 5:2))

referenceTable <- data.frame(monoisotopicMass=c(1.01, 20.2),
                             relativeIntensityApexToMonoisotopic=c(1, 5/3),
                             apexIdx=c(1, 2))
m <- monoisotopic(p, chargeState=1)
mass(m) # c(1, 20)
}
\seealso{
  \code{\linkS4class{MassPeaks}},
  \code{\linkS4class{MassSpectrum}},
  \code{\link[MALDIquant]{detectPeaks,MassSpectrum-method}},
  \code{\link[MALDIquantTools]{averagineTable}},
  \code{\link[MALDIquantTools]{uniProtTable}}
}
\keyword{methods}

