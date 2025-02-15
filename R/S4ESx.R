## First require necessary packages
library(fs)
library(openssl)  # for sha256 hashing

## ESx class definition
setClass("ESx",
         slots = list(
           path = "fs_path",     # Path to the binary file
           records = "list",     # List of Record objects
           sha256sum = "hash"    # SHA-256 hash of the file
         ),
         prototype = list(
           path = fs::path(),
           records = list(),
           sha256sum = openssl::sha256("")
         ))

## Initialize method for ESx
setMethod("initialize", "ESx",
          function(.Object, filepath, ...) {
            ## Validate and convert the file path
            .Object@path <- fs::path(filepath)

            if (!fs::file_exists(.Object@path)) {
              stop("File does not exist: ", filepath)
            }

            ## Calculate SHA-256 hash of the file
            .Object@sha256sum <- openssl::sha256(file(.Object@path, "rb"))

            ## Open connection and read records
            con <- file(.Object@path, "rb")
            on.exit(close(con))

            records <- list()

            ## Read until we reach end of file
            while (TRUE) {
              ## Get current position
              current_pos <- seek(con)

              ## Try to read 4 bytes (for record name)
              test_read <- try(readBin(con, "raw", n = 4), silent = TRUE)

              ## If we couldn't read 4 bytes, we're at EOF
              if (inherits(test_read, "try-error") || length(test_read) < 4) {
                break
              }

              if (rawToChar(test_read) != "TES3") {
                stop("Not a valid ESx file: Missing TES3 signature")
              }

              ## Seek back to start of record
              seek(con, current_pos)

              ## Create new record (this will automatically seek past the record data)
              record <- new("Record", con, offset = current_pos)
              records <- c(records, list(record))
            }

            .Object@records <- records
            .Object
          })

## Method to get a record by name
setGeneric("getRecord", function(object, name) standardGeneric("getRecord"))

setMethod("getRecord", "ESx",
          function(object, name) {
            ## Find record with matching name
            matching_records <- Filter(function(r) r@name == name, object@records)

            if (length(matching_records) == 0) {
              return(NULL)
            }

            ## Return first matching record
            matching_records[[1]]
          })

## Method to verify file hasn't changed
setGeneric("verifyChecksum", function(object) standardGeneric("verifyChecksum"))

setMethod("verifyChecksum", "ESx",
          function(object) {
            current_sha256 <- openssl::sha256(file(object@path, "rb"))
            return(current_sha256 == object@sha256sum)
          })

## Example usage:
## esx_file <- new("ESx", "example.bin")
## record <- getRecord(esx_file, "HEDR")
## is_unchanged <- verifyChecksum(esx_file)

setMethod("verifyRecordCount", "ESx",
          function(object) {
            ## Find index of TES3 record
            tes3_index <- which(sapply(object@records, function(r) r@name == "TES3"))
            if (length(tes3_index) == 0) {
              stop("Cannot verify record count: TES3 record not found")
            }

            ## Read the record if not already read
            if (length(object@records[[tes3_index]]@subrecords) == 0) {
              con <- file(object@path, "rb")
              on.exit(close(con))
              object@records[[tes3_index]] <- readRecord(object@records[[tes3_index]], con)
            }

            ## Find HEDR subrecord
            hedr <- Filter(function(sr) sr@name == "HEDR",
                           object@records[[tes3_index]]@subrecords)[[1]]

            ## Read subrecord if not already read
            if (length(hedr@data) == 0) {
              con <- file(object@path, "rb")
              on.exit(close(con))
              hedr <- readSubrecord(hedr, con)
            }

            ## Skip to the last 4 bytes of HEDR data
            record_count <- readBin(hedr@data[(300-3):300], "integer", n=1, size=4, endian="little")

            ## Compare with actual record count (subtract 1 to exclude TES3 record itself)
            actual_count <- length(object@records) - 1

            return(record_count == actual_count)
          })

## Method to parse HEDR data from TES3 record
setGeneric("parseHEDR", function(object) standardGeneric("parseHEDR"))

setMethod("parseHEDR", "ESx",
          function(object) {
            ## Find index of TES3 record
            tes3_index <- which(sapply(object@records, function(r) r@name == "TES3"))
            if (length(tes3_index) == 0) {
              stop("Cannot parse HEDR: TES3 record not found")
            }

            ## Read TES3 record if not already read
            if (length(object@records[[tes3_index]]@subrecords) == 0) {
              con <- file(object@path, "rb")
              on.exit(close(con))
              object@records[[tes3_index]] <- readRecord(object@records[[tes3_index]], con)
            }

            ## Find index of HEDR subrecord
            hedr_index <- which(sapply(object@records[[tes3_index]]@subrecords,
                                       function(sr) sr@name == "HEDR"))
            if (length(hedr_index) == 0) {
              stop("Cannot parse HEDR: HEDR subrecord not found")
            }

            ## Read HEDR subrecord if not already read
            hedr <- object@records[[tes3_index]]@subrecords[[hedr_index]]
            if (length(hedr@data) == 0) {
              con <- file(object@path, "rb")
              on.exit(close(con))
              object@records[[tes3_index]]@subrecords[[hedr_index]] <-
                readSubrecord(hedr, con)
            }

            ## Get the raw data
            hedr_data <- object@records[[tes3_index]]@subrecords[[hedr_index]]@data

            ## Parse the components
            version <- readBin(hedr_data[1:4], "numeric", n=1, size=4, endian="little")
            unknown <- readBin(hedr_data[5:8], "integer", n=1, size=4, endian="little")

            ## Extract strings (removing null terminators)
            company_name <- rawToChar(hedr_data[9:40])
            company_name <- gsub("\\0*$", "", company_name)  # Remove null padding

            description <- rawToChar(hedr_data[41:296])
            description <- gsub("\\0*$", "", description)  # Remove null padding

            record_count <- readBin(hedr_data[297:300], "integer", n=1, size=4, endian="little")

            ## Return as a list
            list(
              version = version,
              unknown = unknown,
              company_name = company_name,
              description = description,
              record_count = record_count
            )
          })

## Example usage:
## esx_file <- new("ESx", "example.esp")
## header_info <- parseHEDR(esx_file)
## print(header_info$version)
## print(header_info$company_name)
## etc...
