parseMASTSubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(basename = rawToChar(rawData))
  },
  parseError = function(e) {
    warning("Parser error caught during MAST subrecord parsing!")
  })
}

parseDATASubrecordData <- function(rawData, parentRecordHeader) {
  tryCatch({
    list(bytes = readBin(rawData, "integer", size = 8))
  },
  parseError = function(e) {
    warning("Parser error caught during DATA subrecord parsing!")
  })
}

