parseSCRSSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    return(list(unknown = readBin(con, "raw", subrecord@size)))
  },
  parseError = function(e) {
    warning("Parser error caught during SCRS subrecord parsing!")
  })
}
