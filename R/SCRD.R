parseSCRDSubrecordData <- function(rawData) {
  return(list(
    unkown = readBin(rawData, "raw", size = 20)
  ))
}
