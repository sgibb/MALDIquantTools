\docType{data}
\name{averagineTable}
\alias{averagineTable}
\title{Averagine reference table}
\description{
  This dataset contains mass for combinations of averagine.
}
\details{
  To create: \preformatted{ z <-
  read.csv(system.file("averagine.csv",
  package="MALDIquantTools"), stringsAsFactors=FALSE)

  averagineTable <-
  MALDIquantTools:::createAveragineReferenceTable(z,
  minMass=120, maxMass=5000) }
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
  \code{\link[MALDIquantTools]{uniProtTable}},
  \code{\link[MALDIquantTools]{createAveragineReferenceTable}}
}
\keyword{data}
\keyword{datasets}

