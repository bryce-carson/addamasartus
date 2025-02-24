parseGMDTSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    unknownFloats <- readBin(con, "numeric", n = 6, size = 4)
    cellName <- readBin(con, "raw", n = 64)
    unknownFloat <- readBin(con, "numeric", n = 1, size = 4)
    characterName <- readBin(con, "raw", n = 32)

    if (seek(con) - subrecord@offset == subrecord@size) {
      signalCondition(
        errorCondition("The number of bytes read was inequal to the specified
 size of the subrecord.",
 class = "SubrecordReadError")
 )
    }

    return(list(unknownFloats, cellName, unknownFloat, characterName))
  },
  error = function(e) {
    c <- errorCondition("Parser error caught during GMDT subrecord parsing!",
                        class = "SubrecordParserError")
    signalCondition(c)
  })
}
