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

## completely taken from caTools 1.11 R/base64.R
## modification by Sebastian Gibb <mail@sebastiangibb.de>:
## - remove base64decode function
## - prepend a dot to function name
## - add compression support

#===========================================================================#
# caTools - R library                                                       #
# Copyright (C) 2005 Jarek Tuszynski                                        #
# Distributed under GNU General Public License version 3                    #
#===========================================================================#

#===============================================================================
# The Base64 encoding is designed to encode arbitrary binary information for 
# transmission by electronic mail. It is defined by MIME (Multipurpose Internet 
# Mail Extensions) specification RFC 1341, RFC 1421, RFC 2045 and others. 
# Triplets of 8-bit octets are encoded as groups of four characters, each 
# representing 6 bits of the source 24 bits. Only a 65-character subset 
# ([A-Z,a-z,0-9,+,/,=]) present in all variants of ASCII and EBCDIC is used, 
# enabling 6 bits to be represented per printable character
#===============================================================================

.base64encode = function(x, size=NA, endian=.Platform$endian,
                        compressionType=c("none", "gzip"))
{
   if (typeof(x)!="raw") x = writeBin(x, raw(), size=size, endian=endian)

   ## add compression support
   compressionType <- match.arg(compressionType, several.ok=FALSE)
   x <- memCompress(from=x, type=compressionType)

   library(bitops)                # needed for bitops::bitOr and bitops::bitAnd
   x = as.integer(x)
   ndByte = length(x)            # number of decoded bytes
   nBlock = ceiling(ndByte / 3)  # number of blocks/groups
   neByte = 4 * nBlock           # number of encoded bytes

   # add padding if necessary, to make the length of x a multiple of 3
   if (ndByte < 3*nBlock) x[(ndByte+1) : (3*nBlock)] = 0;
   dim(x) = c(3, nBlock)         # reshape the data
   y = matrix(as.integer(0), 4, nBlock)  # for the encoded data
   
   #-------------------------------------------
   # Split up every 3 bytes into 4 pieces
   #   x = aaaaaabb bbbbcccc ccdddddd
   # to form
   #   y = 00aaaaaa 00bbbbbb 00cccccc 00dddddd
   # than convert y to integers in 0-63 range
   # This section is based on Matlab code by Peter Acklam
   # http://home.online.no/~pjacklam/matlab/software/util/datautil/
   #-------------------------------------------
   y[1,] = bitops::bitShiftR(x[1,], 2) # 6 highest bits of x(1,:)
   y[2,] = bitops::bitOr(bitops::bitShiftL(x[1,], 4), bitops::bitShiftR(x[2,], 4))
   y[3,] = bitops::bitOr(bitops::bitShiftL(x[2,], 2), bitops::bitShiftR(x[3,], 6))
   y[4,] = x[3,]
   y = bitops::bitAnd(y, 63)           # trim numbers to lower 6-bits
   
   #----------------------------------
   # Perform the following mapping
   #   0  - 25  ->  A-Z
   #   26 - 51  ->  a-z
   #   52 - 61  ->  0-9
   #   62       ->  +
   #   63       ->  /
   #----------------------------------
   alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
   alpha = strsplit(alpha, NULL)[[1]]  # convert string to array of characters
   z = alpha[y+1]                      # rearrange characters 

   #-------------------------
   # Add padding if necessary.
   #-------------------------
   npbytes = 3 * nBlock - ndByte   # number of padding bytes needed
   if (npbytes>0) z[(neByte-npbytes+1) : neByte] = '='  # '=' is used for padding
   z = paste(z, collapse = "")       # combine characters into a string
   return (z)
}

