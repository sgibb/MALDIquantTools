\name{.readMzRFile}
\alias{.readMzRFile}
\title{Reads mass spectrometry files}
\usage{
  .readMzRFile(file, verbose = FALSE, ...)
}
\arguments{
  \item{file}{file path}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to
  \code{\link[mzR]{openMSfile}}}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  This function reads a mass spectrometry file into a
  \code{\link[MALDIquant]{MassSpectrum-class}} object. All
  formats supported by \code{\link[mzR]{mzR-package}} could
  read in.
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum}},
  \code{\link[mzR]{openMSfile}}
}
\keyword{internal}

