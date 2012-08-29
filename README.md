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
  [mzR](http://bioconductor.org/packages/release/bioc/html/mzR.html)), ASCII and CSV files
- Export in ASCII, CSV, [mMass'](http://www.mmass.org) MSD files
- Moving average
- Convert a matrix to a binary one (replace `NA` by `0` and `!NA` by `1`)
- Find reference peaks in each individual spectrum.
- Find monoisotopic peaks in spectra.
