## Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

#' Create reference table
#' 
#' This function creates an averagine reference table for
#' \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}} at mass range
#' \code{minMass} to \code{maxMass}.
#' 
#' @param averagine data.frame (columns: \code{mass, C, H, N, O, S}).
#' @param minMass double, minimal mass.
#' @param maxMass double, maximal mass.
#' @return data.frame
#'
#' @references
#'  Michael W. Senko and Steven C. Beu and Fred W. McLafferty (1995), \cr
#'  \dQuote{Determination of monoisotopic masses and ion populations for large
#'  biomolecules from resolved isotopic distributions} \cr
#'  \emph{Journal of the American Society for Mass Spectrometry}, \bold{4} (6): 229-233 \cr
#'  ISSN: 1044-0305; doi 10.1016/1044-0305(95)00017-8 \cr
#'  \url{http://www.sciencedirect.com/science/article/pii/1044030595000178}
#
#' @seealso \code{\link[MALDIquantTools]{averagineTable}},
#'  \code{\link[MALDIquantTools]{uniProtTable}},
#'  \code{\link[MALDIquantTools]{createUniProtReferenceTable}}
#' @keywords internal
#' @rdname createAveragineReferenceTable-functions
#'
createAveragineReferenceTable <- function(averagine, minMass=120, maxMass=5000) {
  stopifnot(require("BRAIN"))

  n <- floor(minMass/averagine$mass) : ceiling(maxMass/averagine$mass)
      
  df <- data.frame()

  for (i in n) {
    l <- lapply(averagine[, c("C", "H", "N", "O", "S")], function(x)x*i)

    r <- .useBRAIN(l)

    if (!is.na(r$monoisotopicMass)) {
      df <- rbind(df, r)
    }
  }
  return(df)
}

#' Create reference table
#' 
#' This function creates an UniProt reference table for
#' \code{\link[MALDIquantTools]{monoisotopic,MassPeaks-method}}.
#' 
#' @param file file path to downloaded table (if missing an automatically
#'  download is started).
#' @param organism numeric, organism in UniProt Database.
#' @param keyword numeric, keyword in UniProt Database.
#' @param reviewed logical, if missing both reviewed and non reviewed proteins
#'  are downloaded.
#' @return data.frame
#'
#' @references
#'  \url{http://www.uniprot.org/}
#' 
#' @seealso \code{\link[MALDIquantTools]{uniProtTable}},
#'  \code{\link[MALDIquantTools]{averagineTable}},
#'  \code{\link[MALDIquantTools]{createAveragineReferenceTable}}
#' @keywords internal
#' @rdname createUniProtReferenceTable-functions
#'
createUniProtReferenceTable <- function(file, organism=9606, keyword=181, 
                                        reviewed,
                                        aminoacids=c("A", "R", "N", "D", "C",
                                                     "E", "Q", "G", "H", "I",
                                                     "L", "K", "M", "F", "P", 
                                                     "S", "T", "W", "Y", "V"),
                                        progressBar=TRUE) {
  stopifnot(require("BRAIN"))

  if (missing(file)) {
    ## create download url
    url <- paste("http://www.uniprot.org/uniprot/?query=organism%3a", organism,
                 "+keyword%3a", keyword,
                 ifelse(missing(reviewed), "",
                        paste("+reviewed%3a", ifelse(reviewed, "yes", "no"),
                              sep="")),
                 "&force=yes&format=tab&columns=id,sequence", sep="")

    f <- tempfile()

    download.file(url, f)
  } else {
    f <- file
  }
                                                                                                                                                        
  ## read table
  uniprot <- read.table(f, sep="\t", header=TRUE)

  ## remove spaces
  uniprot$Sequence <- gsub(" ", "", uniprot$Sequence)

  n <- nrow(uniprot)

  if (progressBar) {
    pb <- txtProgressBar(0, n, style=3)
  }

  df <- data.frame()

  for (i in 1:n) {
    ## from BRAIN vignette
    aa <- as.character(uniprot[i, "Sequence"])
    seqVector <- strsplit(aa, split="")[[1]]

    if (!is.na(sum(match(seqVector, aminoacids)))) {
      l <- BRAIN::getAtomsFromSeq(aa)

      r <- .useBRAIN(l)

      if (length(r$monoisotopicMass)) {
        df <- rbind(df, r)
      }
    }

    if (progressBar) {
      setTxtProgressBar(pb, i)
    }
  }

  if (progressBar) {
    close(pb)
  }

  if (missing(file)) {
    unlink(f)
  }

  return(df[order(df$monoisotopicMass), ])
}
 
#' This function is a wrapper around \code{\link[BRAIN]{useBRAIN}}.
#' 
#' @param aC List with fields C, H, N, O, S of integer non-negative values.
#' @return data.frame
#' 
#' @seealso \code{\link[BRAIN]{useBRAIN}}
#' @keywords internal
#' @rdname useBRAIN-functions
#'
.useBRAIN <- function(aC) {
  r <- BRAIN::useBRAIN(aC=aC, nrPeaks=1000, stopOption="abundantEstim",
                       abundantEstim=10)

  maxIdx <- which.max(r$isoDistr)
  monoIdx <- which(r$masses == r$monoisotopicMass)
  monoisotopicMass <- r$monoisotopicMass
  relativeIntensitySecondToMonoisotopic <-
    r$isoDistr[monoIdx+1]/r$isoDistr[monoIdx]
  relativeIntensityApexToMonoisotopic <- 
    r$isoDistr[maxIdx]/r$isoDistr[monoIdx]
  if (is.na(r$monoisotopicMass)) {
    return(data.frame())
  } else {
    return(data.frame(monoisotopicMass=monoisotopicMass,
                      apexIdx=maxIdx,
                      relativeIntensityApexToMonoisotopic,
                      relativeIntensitySecondToMonoisotopic))
  }
}

