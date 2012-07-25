\name{isBinaryMatrix}
\alias{isBinaryMatrix}
\title{Tests whether given matrix is a binary one.}
\usage{
  isBinaryMatrix(x)
}
\arguments{
  \item{x}{matrix}
}
\value{
  TRUE/FALSE
}
\description{
  Tests whether given matrix is a binary one.
}
\examples{
m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)
isBinaryMatrix(m)    # FALSE
isBinaryMatrix(1:10) # FALSE
isBinaryMatrix(b)    # TRUE
}

