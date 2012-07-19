\name{.readMzAsciiFile}
\alias{.readMzAsciiFile}
\title{Reads mass spectrometry files}
\usage{
  .readMzAsciiFile(file, verbose = FALSE, ...)
}
\arguments{
  \item{file}{file path}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to
  \code{\link{read.table}}}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  This functions reads mass spectrometry data stored in
  ASCII text files into
  \code{\link[MALDIquant]{MassSpectrum-class}} object.
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum}},
  \code{\link{read.table}}
}
\keyword{internal}

