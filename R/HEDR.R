parseHEDRSubrecordData <- function(rawData) {
  ## Parse the components
  version <- readBin(rawData[1:4], "numeric", n = 1, size = 4, endian = "little")
  unknown <- readBin(rawData[5:8], "integer", n = 1, size = 4, endian = "little")
  ## Extract strings (removing null terminators)
  company_name <- gsub("\\0*$", "", rawToChar(rawData[9:40])) # Remove null padding
  description <- gsub("\\0*$", "", rawToChar(rawData[41:296])) # Remove null padding
  record_count <- readBin(rawData[297:300], "integer", n = 1, size = 4, endian = "little")
  ## Return as a list
  data <- list(
    version = version,
    unknown = unknown,
    company_name = company_name,
    description = description,
    record_count = record_count
  )

  data
}
