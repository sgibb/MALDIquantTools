\name{createAveragineReferenceTable}
\alias{createAveragineReferenceTable}
\title{Create reference table}
\usage{
  createAveragineReferenceTable(averagine, minMass = 120,
    maxMass = 5000)
}
\arguments{
  \item{averagine}{data.frame (columns: \code{mass, C, H,
  N, O, S}).}

  \item{minMass}{double, minimal mass.}

  \item{maxMass}{double, maximal mass.}
}
\value{
  data.frame
}
\description{
  This function creates an averagine reference table for
  \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}
  at mass range \code{minMass} to \code{maxMass}.
}
\references{
  Michael W. Senko and Steven C. Beu and Fred W. McLafferty
  (1995), \cr \dQuote{Determination of monoisotopic masses
  and ion populations for large biomolecules from resolved
  isotopic distributions} \cr \emph{Journal of the American
  Society for Mass Spectrometry}, \bold{4} (6): 229-233 \cr
  ISSN: 1044-0305; doi 10.1016/1044-0305(95)00017-8 \cr
  \url{http://www.sciencedirect.com/science/article/pii/1044030595000178}
}
\seealso{
  \code{\link[MALDIquantTools]{averagineTable}},
  \code{\link[MALDIquantTools]{uniProtTable}},
  \code{\link[MALDIquantTools]{createUniProtReferenceTable}}
}
\keyword{internal}

