\name{readDirectory}
\alias{readDirectory}
\title{Reads all files in one directory matching a given pattern}
\usage{
  .readDirectory(path, pattern, fun, verbose = FALSE, ...)
}
\arguments{
  \item{path}{path to directory}

  \item{pattern}{file pattern to look for}

  \item{fun}{function which should used to read the a
  single file}

  \item{verbose}{verbose output?}

  \item{...}{further arguments passed to \code{fun}}
}
\value{
  list of MALDIquant::MassSpectrum objects
}
\description{
  Reads all files in one directory matching a given
  pattern. WARNING: this is a recursive function!
}
\seealso{
  \code{\link[MALDIquant]{MassSpectrum-class}}
}
\keyword{internal}

