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

#' This function draws a simple pseudo-gelmap.
#' 
#' @param x a list of \code{\linkS4class{MassSpectrum}} or a list of
#'  \code{\linkS4class{MassPeaks}} objects.
#' @param col gelmap colours.
#' @param rowLabels labels of rows.
#' @param colLabels labels of columns.
#' @param dendrogram a \code{\link[stats]{dendrogram}} which is plotted on the
#'  left of the gelmap.
#' @param dendrogramRatio ratio of width of dendrogram to gelmap.
#' @param xlab a label for the x axis.
#' @param \ldots further arguments passed to \code{\link[graphics]{image}}.
#'
#' @export
#' @seealso \code{\linkS4class{MassPeaks}}, \code{\linkS4class{MassSpectrum}}
#' @rdname gelmap
#' @examples
#' library("MALDIquant")
#' library("MALDIquantTools")
#' data(fiedler2009subset)
#'
#' ## select some tumor/control subset
#' spectra <- fiedler2009subset[9:16]
#' 
#' ## run preprocessing
#' spectra <- transformIntensity(spectra, sqrt)
#' spectra <- transformIntensity(spectra, movingAverage, halfWindowSize=2)
#' spectra <- removeBaseline(spectra)
#' spectra <- standardizeTotalIonCurrent(spectra)
#' peaks <- detectPeaks(spectra)
#' peaks <- binPeaks(peaks)
#'
#' ## create labels
#' tumor <- sapply(peaks, function(x)grepl(pattern="tumor", x=metaData(x)$file))
#' fullName <- sapply(peaks, function(x)metaData(x)$fullName)
#' rowLabels <- paste(fullName, " (", ifelse(tumor, "T", "C"), ")", sep="")
#'
#' ## plot gelmap
#' gelmap(peaks, rowLabels=rowLabels)
#'
#' ## run clustering and create dendrogram
#' iM <- intensityMatrix(peaks)
#' iM[is.na(iM)] <- 0
#' d <- dist(iM, method="euclidean")
#' d <- as.dendrogram(hclust(d, method="complete"), hang=-1)
#'
#' ## plot gelmap
#' gelmap(peaks, rowLabels=rowLabels, dendrogram=d)
#'
gelmap <- function(x, col=gray((255:1)/255), rowLabels, colLabels,
                   dendrogram, dendrogramRatio=1/5, xlab="mass", ...) {
  ## handle arguments
  optArgs <- list(...);

  if (MALDIquant::isMassSpectrumList(x)) {
    arguments <- list(x=x)

    if (MALDIquant:::.isArgument("nbins", optArgs)) {
      arguments$nbins <- optArgs$nbins
      optArgs <- MALDIquant:::.removeArguments("nbins", optArgs)
    } else {
      arguments$nbins <- median(unlist(lapply(x, length)))/10
    }

    x <- do.call(.binSpectrum, arguments)
  }

  if (MALDIquant::isMassObjectList(x)) {
    x <- MALDIquant::intensityMatrix(x)
  }

  stopifnot(is.matrix(x))

  ## backup graphical settings and restore them later
  parSettings <- par(no.readonly=TRUE)
  on.exit(par(parSettings))
  
  nr <- nrow(x)
  nc <- ncol(x)

  idx <- 1:nr

  ## plot dendrogram
  if (!missing(dendrogram)) {
    layout(t(as.matrix(1:2)), widths=c(1, 1/dendrogramRatio),
           heights=rep(1, 2))
    par(mai=c(1, 0.25, 0.8, 0))

    if (!inherits(dendrogram, "dendrogram")) {
      dendrogram <- as.dendrogram(dendrogram, hang=-1)
    }

    if (!missing(rowLabels)) {
      newOrder <- sort(rowLabels, index.return=TRUE, decreasing=TRUE)$ix
      dendrogram <- reorder(dendrogram, newOrder)
    }

    idx <- order.dendrogram(dendrogram)
    plot(dendrogram, axes=FALSE, ann=FALSE, horiz=TRUE, xaxs="i",
         leaflab="none", ylim=c(1, nr))
  }

  x <- x[idx, ]

  ## plot gelmap
  leftMargin <- ifelse(missing(dendrogram), 0.25, 0)

  if (!missing(rowLabels)) {
    maxLabelWidth <- max(sapply(rowLabels, strwidth, units="inch"))
    par(mai=c(1, leftMargin, 0.8, 0.25+maxLabelWidth))
  } else {
    par(mai=c(1, leftMargin, 0.8, 0))
  }

  arguments <- list(x=1:nc, y=1:nr, z=t(x), col=col, ylab="", xlab=xlab,
                    axes=FALSE, add=FALSE)
  arguments <- c(arguments, optArgs)

  do.call(image, arguments)

  ## plot y axis
  if (!missing(rowLabels)) {
    rowLabels <- rowLabels[idx]
    axis(4, at=axTicks(4, axp=c(1, nr, nr-1)), labels=rowLabels, tick=FALSE,
         las=1)
  }

  ## plot x axis
  if (missing(colLabels)) {
    ticks <- axTicks(1, axp=c(1, nc, parSettings$xaxp[3]))
    mass <- as.double(colnames(x)[ticks])
    ## round to nearest 100
    mass <- floor((mass + 50) / 100) * 100;
    colLabels <- as.character(mass)
  }
  axis(1, at=axTicks(1, axp=c(1, nc, length(colLabels)-1)), labels=colLabels)
}

