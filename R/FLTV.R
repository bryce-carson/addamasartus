parseFLTVSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    return(list(
      floatData = readBin(con, "numeric", subrecord@size)
    ))
  },
  error = function(e) {
    errorCondition("Parse error caught during HEDR subrecord parsing!",
                   class = "parseError")
  })
}
