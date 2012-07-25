\name{binaryMatrix}
\alias{binaryMatrix}
\title{Converts a matrix to binary (0/1). \code{NA} becomes zero and \code{!NA}
becomes one.}
\usage{
  binaryMatrix(x)
}
\arguments{
  \item{x}{matrix}
}
\value{
  matrix
}
\description{
  Converts a matrix to binary (0/1). \code{NA} becomes zero
  and \code{!NA} becomes one.
}
\examples{
m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
binaryMatrix(m)
#     [,1] [,2]
#[1,]    1    0
#[2,]    1    1
}

