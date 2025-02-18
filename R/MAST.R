parseMASTSubrecordData <- function(rawData) {
  list(basename = rawToChar(rawData))
}

parseDATASubrecordData <- function(rawData) {
  list(bytes = readBin(rawData, "integer", size = 8))
}
