parseSCRSSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(unknown = rawData)
  },
  parseError = function(e) {
    warning("Parser error caught during SCRS subrecord parsing!")
  })
}
