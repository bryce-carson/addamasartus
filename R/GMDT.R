parseGMDTSubrecordData <- function(rawData) {
  data <- list(
    unknownFloats = readBin(rawData, "double", n = 6, size = 4),
    cellName = readBin(rawData, "character", size = 64),
    unknownFloat = readBin(rawData, "double", n = 1, size = 4),
    characterName = readBin(rawData, "character", size = 32)
  )

  return(data)
}
