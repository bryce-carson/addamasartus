## ESx class definition
setClass("ESx",
         slots = list(path = "fs_path", # Path to the binary file
                      records = "list", # List of Record objects
                      sha256sum = "SHA256HASH"), # SHA-256 hash of the file
         prototype = list(path = fs::path(),
                          records = list(),
                          sha256sum = openssl::sha256("")))

setGeneric("read", function(x, ..., lazy = TRUE) standardGeneric("read"),
           signature = "x")
setMethod("read", "ESx", function(x, ..., lazy = TRUE) {
  ## Open connection and read records
  con <- file(x@path, "rb")
  on.exit(close(con))

  if (lazy) {
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

      ## If reading of the connection has just begun, ensure that the last four
      ## bytes read (a record header name) are the magic bytes for this file type.
      if (seek(con) == 4 && (isTES3Record <- rawToChar(test_read) != "TES3")) {
        stop("Not a valid ESx file: Missing TES3 signature")
      } else if (isTES3Record) {
        TES3 <- new("Record", con, offset = current_pos)
        read()
      } else {
      }

      ## Seek back to start of record
      seek(con, current_pos)

      ## Create new record (this will automatically seek past the record data)
      record <- new("Record", con, offset = current_pos)
      records <- c(records, list(record))
    }

    x@records <- records
  }
})

ESx <- function(x, filepath) {
  ## Calculate SHA-256 hash of the file
  x@sha256sum <- openssl::sha256(file(x@path, "rb"))

  new("ESx", filepath)
}

## This only validates the object's file path, nothing else.
setValidity("ESx", function(object) {
  all(ifelse(c(methods::is(.Object@path, "fs_path"),
               fs::file_exists(.Object@path)),
             yes = TRUE,
             no = c(
               sprintf("@path must be an fs_path object!", is(.Object@path)),
               sprintf("@path does not exist (%s)!", .Object@path)
             )))
})

## Method to get a record by name
setGeneric("getRecord", function(object, name) standardGeneric("getRecord"))
## TODO: custom indentation rules in the language server and in Emacs for this.
setMethod("getRecord", "ESx",
          function(object, name) {
            ## TODO: what is the definition of this Filter function? Was it a ChatGPT
            ## hallucination? Find record with matching name
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
setGeneric("verifyRecordCount", function(object) standardGeneric("verifyRecordCount"))
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

## Example usage:
## esx_file <- new("ESx", "example.esp")
## header_info <- parseHEDR(esx_file)
## print(header_info$version)
## print(header_info$company_name)
## etc...
