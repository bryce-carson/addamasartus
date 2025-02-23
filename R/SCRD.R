parseSCRDSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    return(list(
      unkown = rawData
    ))
  },
  parseError = function(e) {
    warning("Parser error caught during SCRD subrecord parsing!")
  })
}
