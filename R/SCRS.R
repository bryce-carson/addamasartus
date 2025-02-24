#' @name Parse Screenshot (SCRS) subrecord data
#' @return a PNG
parseSCRSSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    screenshot_bytes <- readBin(con, "raw", subrecord@size)
    BGR <- matrix(screenshot_bytes, subrecord@size / 4, 4, byrow = TRUE,
                  dimnames = list(NULL, c("BB", "GG", "RR", "AA")))
    screenshotArray <- array(c(c(BGR[, 3]),
                               c(BGR[, 2]),
                               c(BGR[, 1]),
                               c(BGR[, 4])),
                             dim = c(64, 64, 4))
    ## TODO: utilize imager for very simple image manipulation and
    ## post-processing.
    ## screenshot <- imager::as.cimg(screenshotArray)

    return(list("array" = screenshotArray))
  },
  parseError = function(e) {
    warning("Parser error caught during SCRS subrecord parsing!")
  })
}

## Translated from the Mopy source code. TODO: prefer using imager or magick
## functions instead, after successful plotting/rendering of the image as it is
## encoded.
colourRemap <- function(image) {
  averageRGB <- mean()


  return(apply(image, 2, function(colour) {

    colour <- (colour - averageRGB) * scaleRGB
    return(max(0, min(255, colour + 128)))
  }))
}

## Hand translated from Python 2 from Mopy/mash/mosh.py (https://github.com/polemion/Wrye-Mash-Polemos/blob/dbce232bd053ff8e2c49e6ada85c570f07376d7d/Mopy/mash/mosh.py#L3760-L3782).
## #--Convert bgra array to rgb array
## buff = cStringIO.StringIO()
## for num in xrange(len(subrecord.data)/4):
##     bb,gg,rr = struct.unpack('3B',subrecord.data[num*4:num*4+3])
##     buff.write(struct.pack('3B',rr,gg,bb))
## rgbString = buff.getvalue()
## try: #--Image processing (brighten, increase range)
##     rgbArray = array.array('B',rgbString)
##     rgbAvg   = float(sum(rgbArray))/len(rgbArray)
##     rgbSqAvg = float(sum(xx*xx for xx in rgbArray))/len(rgbArray)
##     rgbSigma = math.sqrt(rgbSqAvg - rgbAvg*rgbAvg)
##     rgbScale = max(1.0,80/rgbSigma)
##     def remap(color):
##         color = color - rgbAvg
##         color = color * rgbScale
##         return max(0,min(255,int(color+128)))
## except: pass
## buff.seek(0)
## try: [buff.write(struct.pack('B',remap(ord(char)))) for num,char in enumerate(rgbString)]
## except: pass
## screenshot = buff.getvalue()
## buff.close()
## break
