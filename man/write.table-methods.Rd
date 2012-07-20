\docType{methods}
\name{write.table}
\alias{export}
\alias{export,AbstractMassObject-method}
\alias{exportCsv}
\alias{exportCsv,AbstractMassObject-method}
\alias{exportCsv,list-method}
\alias{export,list-method}
\alias{write.csv}
\alias{write.csv,AbstractMassObject-method}
\alias{write.csv,list-method}
\alias{write.table}
\alias{write.table,AbstractMassObject-method}
\alias{write.table,list-method}
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

  \item{path}{directory path}

  \item{force}{create \code{path} if not exists}

  \item{ending}{file type (e.g. "txt", "csv", ...)}

  \item{...}{further arguments passed to
  \code{\link{write.table}}}
}
\description{
  This methods prints a
  \code{\linkS4class{AbstractMassObject}} object to a file
  or connection.
}
\seealso{
  \code{\linkS4class{AbstractMassObject}},
  \code{\link{write.table}}
}
\keyword{methods}

