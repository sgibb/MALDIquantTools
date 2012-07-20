\docType{methods}
\name{write.table}
\alias{write.csv,AbstractMassObject-method}
\alias{write.table}
\alias{write.table,AbstractMassObject-method}
\title{Data output}
\arguments{
  \item{x}{a \code{\linkS4class{AbstractMassObject}}
  object}

  \item{file}{filename}

  \item{row.names}{either a logical value indicating
  whether the row names of \code{x} are to be written along
  with \code{x}, or a character vector of row names to be
  written.}

  \item{col.names}{either a logical value indicating
  whether the column names of \code{x} are to be written
  along with \code{x}, or a character vector of column
  names to be written.  See the section on \code{CSV files}
  for the meaning of \code{col.names = NA}.}

  \item{sep}{the field separator string.}

  \item{...}{further arguments passed to
  \code{\link{write.table}}}
}
\description{
  This method prints a
  \code{\linkS4class{AbstractMassObject}} object to a file
  or connection.
}
\seealso{
  \code{\linkS4class{AbstractMassObject}},
  \code{\link{write.table}}
}

