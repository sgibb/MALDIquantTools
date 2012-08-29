\name{.whichClosest}
\alias{.whichClosest}
\title{Find nearest index}
\usage{
  .whichClosest(db, key)
}
\arguments{
  \item{db}{double, database which contains reference
  values (sorted)}

  \item{key}{double, value to look for}
}
\value{
  nearest index
}
\description{
  This function looks for closest index to a given value.
}
\examples{
library("MALDIquantTools")
db <- 1:10+0.2
MALDIquantTools:::.whichClosest(db, 2.1) # == 2
}
\keyword{internal}

