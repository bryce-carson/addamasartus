parseNAMESubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(
      globalID = rawToChar(rawData)
    )
  },
  error = function(e) {
    errorCondition("Parse error caught during HEDR subrecord parsing!",
                   class = "parseError")
  })
}
