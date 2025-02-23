parseFNAMSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(
      globalDataType = rawToChar(rawData)
    )
  },
  error = function(e) {
    errorCondition("Parse error caught during HEDR subrecord parsing!",
                   class = "parseError")
  })
}
