\name{createUniProtReferenceTable}
\alias{createUniProtReferenceTable}
\title{Create reference table}
\usage{
  createUniProtReferenceTable(file, organism = 9606,
    keyword = 181, reviewed,
    aminoacids = c("A", "R", "N", "D", "C", "E", "Q", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V"),
    progressBar = TRUE)
}
\arguments{
  \item{file}{file path to downloaded table (if missing an
  automatically download is started).}

  \item{organism}{numeric, organism in UniProt Database.}

  \item{keyword}{numeric, keyword in UniProt Database.}

  \item{reviewed}{logical, if missing both reviewed and non
  reviewed proteins are downloaded.}
}
\value{
  data.frame
}
\description{
  This function creates an UniProt reference table for
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}.
}
\references{
  \url{http://www.uniprot.org/}
}
\seealso{
  \code{\link[MALDIquantTools]{uniProtTable}},
  \code{\link[MALDIquantTools]{averagineTable}},
  \code{\link[MALDIquantTools]{createAveragineReferenceTable}}
}
\keyword{internal}

