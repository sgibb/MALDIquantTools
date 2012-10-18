# MALDIquantTools

## Description

Additional tools for [MALDIquant](http://strimmerlab.org/software/maldiquant/)
This package is **not** intended for
public release on [CRAN](http://cran.r-project.org/).

## Install

```R
install.packages("devtools")
library("devtools")
install_github("MALDIquantTools", "sgibb")
```

## Features

- Import of mzData, mzML (supported by
  [mzR](http://bioconductor.org/packages/release/bioc/html/mzR.html)), ASCII and CSV files.
- Export in ASCII, CSV, [mMass'](http://www.mmass.org) MSD files.
- Moving average.
- Calculate FWHM. 
- Noise estimation: Friedman's super smoother
- Find reference peaks in each individual spectrum.
- Find monoisotopic peaks in spectra.
- Draw gelmaps 

## Gelmap Example
```R
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

## run clustering and create dendrogram
iM <- intensityMatrix(peaks)
iM[is.na(iM)] <- 0
d <- dist(iM, method="euclidean")
d <- as.dendrogram(hclust(d, method="complete"), hang=-1)

## plot gelmap
gelmap(peaks, rowLabels=rowLabels, dendrogram=d, xlab="mass [Da]")
```
![gelmap](https://github.com/sgibb/MALDIquantTools/raw/master/images/gelmap.png)
