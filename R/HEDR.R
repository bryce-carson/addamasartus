parseHEDRSubrecordData <- function(con, subrecord, parentRecordHeader) {
  tryCatch({
    ## Parse the components
    version <- readBin(con, "numeric", n = 1, size = 4, endian = "little")
    unknown <- readBin(con, "integer", n = 1, size = 4, endian = "little")

    ## Extract strings (removing null terminators)
    company_name <- gsub("\\0*$", "", rawToChar(readBin(con, "raw", n = 32)))
    description <- gsub("\\0*$", "", rawToChar(readBin(con, "raw", n = 256)))

    record_count <- readBin(con, "integer", n = 1, size = 4, endian = "little")

    if (0 > record_count) {
      errorCondition(sprintf("Record count is negative (%d)!", record_count),
                     class = "TES3HEDRParseError")
    }

    return(list(
      version = version,
      unknown = unknown,
      company_name = company_name,
      description = description,
      record_count = record_count
    ))
  },
  TES3HEDRParseError = \(c) signalCondition(c))
}
