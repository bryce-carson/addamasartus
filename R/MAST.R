parseMASTSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    return(list(basename = rawToChar(readBin(con, "raw", subrecord@size))))
  },
  parseError = function(e) {
    warning("Parser error caught during MAST subrecord parsing!")
  })
}

parseDATASubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    return(list(bytes = readBin(con, "integer", size = subrecord@size)))
  },
  parseError = function(e) {
    warning("Parser error caught during DATA subrecord parsing!")
  })
}

