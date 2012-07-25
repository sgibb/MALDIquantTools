## Kontakt:
## Sebastian Gibb
## <mail@sebastiangibb.de>

#' Converts a matrix to binary (0/1). \code{NA} becomes zero and \code{!NA}
#' becomes one.
#' 
#' @param x matrix
#' @return matrix
#' @export
#' @examples
#' m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
#' binaryMatrix(m)
#' #     [,1] [,2]
#' #[1,]    1    0
#' #[2,]    1    1
binaryMatrix <- function(x) {
  stopifnot(is.matrix(x))

  if (isBinaryMatrix(x)) {
    warning(sQuote("x"), " is already a binary matrix.")
    return(x)
  }

  na <- is.na(x)
  x[na] <- 0
  x[!na] <- 1

  return(x)
}

#' Tests whether given matrix is a binary one.
#' 
#' @param x matrix
#' @return TRUE/FALSE
#' @export
#' @examples
#' m <- matrix(c(1, 2, NA, 2), nrow=2, ncol=2)
#' b <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2)
#' isBinaryMatrix(m)    # FALSE
#' isBinaryMatrix(1:10) # FALSE
#' isBinaryMatrix(b)    # TRUE
isBinaryMatrix <- function(x) {
  return(is.matrix(x) && all(x == 1 | x == 0))
}
