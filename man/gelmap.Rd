\name{gelmap}
\alias{gelmap}
\title{This function draws a simple pseudo-gelmap.}
\usage{
  gelmap(x, col = gray((255:1)/255), rowLabels, colLabels,
    dendrogram, dendrogramRatio = 1/5, xlab = "mass", ...)
}
\arguments{
  \item{x}{a list of \code{\linkS4class{MassSpectrum}} or a
  list of \code{\linkS4class{MassPeaks}} objects.}

  \item{col}{gelmap colours.}

  \item{rowLabels}{labels of rows.}

  \item{colLabels}{labels of columns.}

  \item{dendrogram}{a \code{\link[stats]{dendrogram}} which
  is plotted on the left of the gelmap.}

  \item{dendrogramRatio}{ratio of width of dendrogram to
  gelmap.}

  \item{xlab}{a label for the x axis.}

  \item{\ldots}{further arguments passed to
  \code{\link[graphics]{image}}.}
}
\description{
  This function draws a simple pseudo-gelmap.
}
\examples{
library("MALDIquant")
library("MALDIquantTools")
data(fiedler2009subset)

## select some tumor/control subset
spectra <- fiedler2009subset[9:16]

## run preprocessing
spectra <- transformIntensity(spectra, sqrt)
spectra <- transformIntensity(spectra, movingAverage, halfWindowSize=2)
spectra <- removeBaseline(spectra)
spectra <- standardizeTotalIonCurrent(spectra)
peaks <- detectPeaks(spectra)
peaks <- binPeaks(peaks)

## create labels
tumor <- sapply(peaks, function(x)grepl(pattern="tumor", x=metaData(x)$file))
fullName <- sapply(peaks, function(x)metaData(x)$fullName)
rowLabels <- paste(fullName, " (", ifelse(tumor, "T", "C"), ")", sep="")

## plot gelmap
gelmap(peaks, rowLabels=rowLabels)

## run clustering and create dendrogram
iM <- intensityMatrix(peaks)
iM[is.na(iM)] <- 0
d <- dist(iM, method="euclidean")
d <- as.dendrogram(hclust(d, method="complete"), hang=-1)

## plot gelmap
gelmap(peaks, rowLabels=rowLabels, dendrogram=d)
}
\seealso{
  \code{\linkS4class{MassPeaks}},
  \code{\linkS4class{MassSpectrum}}
}

