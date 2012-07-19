\name{mqReadMzR}
\alias{mqReadMzR}
\title{Reads mass spectrometry files}
\usage{
  mqReadMzR(path, pattern = "*.mzML", verbose = FALSE, ...)
}
\arguments{
  \item{path}{path to directory/file}

  \item{pattern}{file pattern to look for}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to
  \code{.readMzRFile}}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  This function reads mass spectrometry files into
  \code{\link[MALDIquant]{MassSpectrum-class}} objects. All
  formats supported by \code{\link[mzR]{mzR-package}} could
  read in.
}
\examples{
\dontrun{s <- mqReadMzR("/home/user/msdata/", pattern="*.mzXML")}
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum}},
  \code{\link[mzR]{mzR-package}}
}
\keyword{IO}

