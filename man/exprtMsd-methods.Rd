\docType{methods}
\name{exportMsd}
\alias{exportMsd}
\alias{exportMsd,list-method}
\alias{exportMsd,MassSpectrum-method}
\title{Data output}
\arguments{
  \item{object}{\code{\linkS4class{MassSpectrum}} object}

  \item{file}{file name}

  \item{peaks}{optional,\code{\linkS4class{MassPeaks}}
  object}
}
\description{
  This methods prints a \code{\linkS4class{MassSpectrum}}
  object to a file or connection in mMass' MSD format.
}
\examples{
\dontrun{
s <- createMassSpectrum(mass=1:5, intensity=6:10,
                        metaData=list(fullName="sample1")
exportMsd(s, "/home/user/msdata/test.msd")
}
}
\references{
  \url{http://www.mmass.org}
}
\seealso{
  \code{\linkS4class{MassSpectrum}}
}
\keyword{methods}

