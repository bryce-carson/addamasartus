parseFLTVSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(
      floatData = readBin(rawData, "numeric")
    )
  },
  error = function(e) {
    errorCondition("Parse error caught during HEDR subrecord parsing!",
                   class = "parseError")
  })
}
