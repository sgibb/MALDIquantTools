### Copyright 2012 Sebastian Gibb
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

.writeMsdDocument <- function(x, file, peaks, encoding="utf-8") {
  ## stop if file isn't writeable
  if (file.access(file, 2) != 0) {
    stop("No permissions to write into ", sQuote(file), "!")
  }

  f <- file(file, open="w", encoding=encoding)

  .xmlHeader(file=f, encoding=encoding)
  
  .catA("<mSD version=\"2.2\">\n\n", file=f)

  .writeMsdDescription(x, file=f)
  .writeMsdSpectrum(x, file=f)

  if (!missing(peaks)) {
    .writeMsdPeakList(peaks, file=f)
  }

  .catA("</mSD>\n", file=f)

  close(f)
}

.xmlHeader <- function(file, encoding="utf-8") {
  cat("<?xml version=\"1.0\" encoding=\"", encoding, "\" ?>\n", 
      file=file, sep="")
}

.writeMsdDescription <- function(x, file) {
  .catA("  <description>\n", file=file)

  .catA("    <title>", .sanitize(.composeFilename(x, ending="msd")),
        "</title>\n", file=file)
  .catA("    <date value=\"", .sanitize(date()), "\" />\n", file=file)
  .catA("    <operator value=\"", .sanitize(x@metaData$owner), "\" />\n",
        file=file)
  .catA("    <contact value=\"", .sanitize(x@metaData$owner), "\" />\n",
        file=file)
  .catA("    <institution value=\"", .sanitize(x@metaData$institution),
        "\" />\n", file=file)
  .catA("    <instrument value=\"", .sanitize(x@metaData$instrument),
        "\" />\n", file=file)
  .catA("    <notes>", .sanitize(paste(x@metaData$comments, collapse="\n")),
        "</notes>\n", file=file)

  .catA("  </description>\n\n", file=file)
}

.writeMsdSpectrum <- function(x, file) {
  polarity <- x@metaData$ionizationMode;

  if (!is.null(polarity)) {
    polarity <- paste(" polarity=\"",
                      ifelse(grepl(pattern="+", x=polarity), "1", "-1"),
                      "\"", sep="")
  } else {
    polarity <- ""
  }

  .catA("  <spectrum ", "points=\"", length(x), "\"",
        polarity, ">\n", file=file)

  .writeMsdArray(x@mass, name="mzArray", file=file)
  .writeMsdArray(x@intensity, name="intArray", file=file)

  .catA("  </spectrum>\n\n", file=file)
}

.writeMsdArray <- function(x, name, file) {
  .catA("    <", name, " precision=\"64\" ", "compression=\"zlib\" ",
        "endian=\"", .Platform$endian, "\">", file=file)
  .catA(.base64encode(as.double(x), size=8, compressionType="gzip"), file=file)
  .catA("</", name, ">\n", file=file)
}

.writeMsdPeakList <- function(x, file) {
  .catA("  <peaklist>\n", file=file)
    for (i in seq(along=x@mass)) {
      .catA("    <peak ",
            "mz=\"", x@mass[i], "\" intensity=\"", x@intensity[i], "\" ",
            "baseline=\"0.000000\" />\n", file=file)
    }
  .catA("  </peaklist>\n\n", file=file)
}

