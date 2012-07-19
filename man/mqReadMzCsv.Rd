\name{mqReadMzCsv}
\alias{mqReadMzCsv}
\title{Reads mass spectrometry files}
\usage{
  mqReadMzCsv(path, pattern = "*.csv", verbose = FALSE,
    sep = ",", header = TRUE, ...)
}
\arguments{
  \item{path}{path to directory/file}

  \item{pattern}{file pattern to look for}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to
  \code{.readMzAsciiFile}}

  \item{sep}{the field separator character (see also
  \code{\link{read.table}})}

  \item{header}{a logical value indicating whether the file
  contains the names of the variables as its first line
  (see also \code{\link{read.table}})}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  This function reads mass spectrometry data stored in CSV
  files into \code{\link[MALDIquant]{MassSpectrum-class}}
  objects. \cr Files have to have two columns (1st: mass
  value, 2nd: intensity value).
}
\note{
  \code{mqReadMzAscii} and \code{mqReadMzCsv} use the same
  internal method but with different defaults.
}
\examples{
\dontrun{s <- mqReadMzCsv("/home/user/msdata/")}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum}},
  \code{\link{mqReadMzAscii}}
}
\keyword{IO}

