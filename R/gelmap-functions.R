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
#' @param x an `intensityMatrix`.
#' @param col gelmap colours.
#' @param rowLabels labels of rows.
#' @param dendrogram a \code{\link[stats]{dendrogram}} which is plotted on the
#'  left of the gelmap.
#' @param dendrogramRatio ratio of width of dendrogram to gelmap.
#' @param xlab a label for the x axis.
#' @param cex.axis magnification to be used for axis annotation (relative to
#'  current setting of \code{cex}).
#' @param \ldots further arguments passed to \code{\link[graphics]{image}}.
#'
#' @export
#' @seealso \code{\link[MALDIquant]{intensityMatrix}}
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
#' spectra <- transformIntensity(spectra, method="sqrt")
#' spectra <- smoothIntensity(spectra, method="MovingAverage", halfWindowSize=2)
#' spectra <- removeBaseline(spectra, method="SNIP")
#' spectra <- calibrateIntensity(spectra, method="TIC")
#' peaks <- detectPeaks(spectra)
#' peaks <- binPeaks(peaks)
#'
#' ## create labels
#' tumor <- sapply(peaks, function(x)grepl(pattern="tumor", x=metaData(x)$file))
#' fullName <- sapply(peaks, function(x)metaData(x)$fullName)
#' rowLabels <- paste(fullName, " (", ifelse(tumor, "T", "C"), ")", sep="")
#'
#' ## run clustering and create dendrogram
#' iM <- intensityMatrix(peaks, spectra)
#' d <- dist(iM, method="euclidean")
#' d <- as.dendrogram(hclust(d, method="complete"), hang=-1)
#'
#' ## plot gelmap
#' gelmap(iM, rowLabels=rowLabels, dendrogram=d, xlab="mass [Da]")
#'
gelmap <- function(x, col=gray((255:1)/255), rowLabels,
                   dendrogram, dendrogramRatio=1/5, xlab="mass", cex.axis=0.75,
                   ...) {
  ## handle arguments
  optArgs <- list(...)

  stopifnot(is.matrix(x))

  ## backup graphical settings and restore them later
  parSettings <- par(no.readonly=TRUE)
  par(cex.axis=cex.axis)
  par(cex.lab=cex.axis)
  on.exit(par(parSettings))

  nr <- nrow(x)
  nc <- ncol(x)

  idx <- 1:nr

  ## plot dendrogram
  if (!missing(dendrogram)) {
    layout(t(as.matrix(1:2)), widths=c(1, 1/dendrogramRatio),
           heights=rep(1, 2))
    par(mai=c(0.85, 0.25, 0.65, 0))

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
  mass <- as.double(colnames(x))
  colLabels <- as.character(round(mass))[seq(1, nc, length.out=10)]
  axis(1, at=axTicks(1, axp=c(1, nc, length(colLabels)-1)), labels=colLabels, las=2)

  invisible(NULL)
}

