# MALDIquantTools

## Description

Additional tools for [MALDIquant](http://strimmerlab.org/software/maldiquant/)
This package is **not** intended for
public release on [CRAN](http://cran.r-project.org/).

## Install

```R
install.packages("devtools")
library("devtools")
install_github("sgibb/MALDIquantTools")
```

## Features

- Calculate FWHM. 
- Find reference peaks in each individual spectrum.
- Find monoisotopic peaks in spectra.
- Draw gelmaps 

## Gelmap Example
```r
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
```
![gelmap](https://github.com/sgibb/MALDIquantTools/raw/master/images/gelmap.png)
