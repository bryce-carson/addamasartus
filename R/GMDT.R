parseGMDTSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    unknownFloatsSize <- 6 * 4
    unknownFloatSize <- 4

    data <- list(
      unknownFloats = readBin(rawData, "double", n = 6, size = 4),
      cellName = readBin(rawData, "raw", n = 64),
      unknownFloat = readBin(rawData, "double", n = 1, size = 4),
      characterName = readBin(rawData, "raw", n = 32)
    )

    cellNameByteIndices <- seq_length(64) + unknownFloatsSize
    cellName <- gsub("\\0*$", "", rawToChar(rawData[cellNameByteIndices]))

    characterNameByteIndices <- seq_length(32) + unknownFloatSize
    characterName <- gsub("\\0*$", "", rawToChar(rawData[characterNameByteIndices]))

    data$cellName <- cellName
    data$characterName <- characterName

    return(data)
  },
  error = function(e) {
    c <- errorCondition("Parser error caught during GMDT subrecord parsing!",
                        class = "SubrecordParserError")
    signalCondition(c)
  })
}
