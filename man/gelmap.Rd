% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gelmap-functions.R
\name{gelmap}
\alias{gelmap}
\title{This function draws a simple pseudo-gelmap.}
\usage{
gelmap(x, col = gray((255:1)/255), rowLabels, dendrogram,
  dendrogramRatio = 1/5, xlab = "mass", cex.axis = 0.75, ...)
}
\arguments{
\item{x}{an `intensityMatrix`.}

\item{col}{gelmap colours.}

\item{rowLabels}{labels of rows.}

\item{dendrogram}{a \code{\link[stats]{dendrogram}} which is plotted on the
left of the gelmap.}

\item{dendrogramRatio}{ratio of width of dendrogram to gelmap.}

\item{xlab}{a label for the x axis.}

\item{cex.axis}{magnification to be used for axis annotation (relative to
current setting of \code{cex}).}

\item{\ldots}{further arguments passed to \code{\link[graphics]{image}}.}
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
spectra <- transformIntensity(spectra, method="sqrt")
spectra <- smoothIntensity(spectra, method="MovingAverage", halfWindowSize=2)
spectra <- removeBaseline(spectra, method="SNIP")
spectra <- calibrateIntensity(spectra, method="TIC")
peaks <- detectPeaks(spectra)
peaks <- binPeaks(peaks)

## create labels
tumor <- sapply(peaks, function(x)grepl(pattern="tumor", x=metaData(x)$file))
fullName <- sapply(peaks, function(x)metaData(x)$fullName)
rowLabels <- paste(fullName, " (", ifelse(tumor, "T", "C"), ")", sep="")

## run clustering and create dendrogram
iM <- intensityMatrix(peaks, spectra)
d <- dist(iM, method="euclidean")
d <- as.dendrogram(hclust(d, method="complete"), hang=-1)

## plot gelmap
gelmap(iM, rowLabels=rowLabels, dendrogram=d, xlab="mass [Da]")

}
\seealso{
\code{\link[MALDIquant]{intensityMatrix}}
}
