parseFNAMSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    return(list(
      globalDataType = rawToChar(readBin(con, "raw", subrecord@size))
    ))
  },
  error = function(e) {
    errorCondition("Parse error caught during HEDR subrecord parsing!",
                   class = "parseError")
  })
}
