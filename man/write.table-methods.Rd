\docType{methods}
\name{write.table}
\alias{write.csv}
\alias{write.table}
\alias{write.table,AbstractMassObject-method}
\title{Data output}
\arguments{
  \item{x}{a \code{\linkS4class{AbstractMassObject}}
  object}

  \item{file}{filename}

  \item{row.names}{either a logical value indicating
  whether the row names of ‘x’ are to be written along with
  ‘x’, or a character vector of row names to be written.}

  \item{col.names}{either a logical value indicating
  whether the column names of ‘x’ are to be written along
  with ‘x’, or a character vector of column names to be
  written.  See the section on ‘CSV files’ for the meaning
  of ‘col.names = NA’.}

  \item{...}{further arguments passed to
  \code{\link{write.table}}}

  \item{x}{a \code{\linkS4class{AbstractMassObject}}
  object}

  \item{file}{filename}

  \item{row.names}{either a logical value indicating
  whether the row names of ‘x’ are to be written along with
  ‘x’, or a character vector of row names to be written.}

  \item{col.names}{either a logical value indicating
  whether the column names of ‘x’ are to be written along
  with ‘x’, or a character vector of column names to be
  written.  See the section on ‘CSV files’ for the meaning
  of ‘col.names = NA’.}

  \item{sep}{the field separator string.}

  \item{...}{further arguments passed to
  \code{\link{write.table}}}
}
\description{
  This method prints a
  \code{\linkS4class{AbstractMassObject}} object to a file
  or connection.

  This method prints a
  \code{\linkS4class{AbstractMassObject}} object to a file
  or connection (in csv format).
}
\seealso{
  \code{\linkS4class{AbstractMassObject}},
  \code{\link{write.table}}

  \code{\linkS4class{AbstractMassObject}},
  \code{\link{write.table}}
}

