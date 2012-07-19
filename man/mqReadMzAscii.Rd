\name{mqReadMzAscii}
\alias{mqReadMzAscii}
\title{Reads mass spectrometry files}
\usage{
  mqReadMzAscii(path, pattern = "*.txt", verbose = FALSE,
    ...)
}
\arguments{
  \item{path}{path to directory/file}

  \item{pattern}{file pattern to look for}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to
  \code{.readMzAsciiFile}}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  This function reads mass spectrometry data stored in
  ASCII text files into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects. \cr
  Files have to have two columns (1st: mass value, 2nd:
  intensity value).
}
\note{
  \code{mqReadMzAscii} and \code{mqReadMzCsv} use the same
  internal method but with different defaults.
}
\examples{
\dontrun{s <- mqReadMzAscii("/home/user/msdata/")}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum}},
  \code{\link{mqReadMzCsv}}
}
\keyword{IO}

