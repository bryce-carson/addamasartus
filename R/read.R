## NOTE: used for ESx files, Records, and Subrecords.
#' @export
setGeneric("read", function(x, con, lazy = TRUE, ...) standardGeneric("read"), signature = "x")

#' @export
setMethod("read", "ESx", function(x, con, lazy = TRUE, enumerate_records = FALSE) {
  ## TODO: prefer fs_bytes approach to limit the ultimate bound of this...
  ## Read until we reach end of file
  repeat {
    ## Get current position
    current_pos <- seek(con)

    ## Try to read 4 bytes (for record name)
    test_read <- try(readBin(con, "raw", n = 4), silent = TRUE)

    ## If we couldn't read 4 bytes, we're at EOF
    if (inherits(test_read, "try-error") || length(test_read) < 4) {
      if (length(test_read) == 0)
        break
      else
        ## TODO: this may be incorrect in the ideal case, where there is simply
        ## no bytes left at all and we attempt to /check/ if there is at least
        ## another record header. Is there an `is.eof(con)` method?
        stop("Bytes (insufficient for even a record header) remain near EOF, but was unable to read them.")
    }

    ## If reading of the connection has just begun, ensure that the last four
    ## bytes read (a record header name) are the magic bytes for this file
    ## type.
    if (seek(con) == 4 && !(rawToChar(test_read) == "TES3")) {
      stop("Not a valid ESx file: Missing TES3 signature")
    } else if (rawToChar(test_read) == "TES3") {
      TES3 <- Record(con, current_pos, lazy = FALSE)
      HEDR <- TES3@subrecords[[1]]
      count <- HEDR@data$record_count

      if (!exists("count") || !is.numeric(count) || 0 > count) {
        print(str(TES3))
        stop("Critical error obtaining record count from HEDR subrecord of TES3 record.")
      }

      if (enumerate_records)
        records <- vector("list", length = 1 + count)
      else
        records <- vector("list", length = 1)

      records[[1]] <- TES3

      recordIndex <- 2
    } else if (enumerate_records) {
      ## Create new records lazily.
      records[[recordIndex]] <- Record(con, current_pos, lazy)
      recordIndex <- recordIndex + 1
    } else {
      break
    }
  }

  x@records <- records

  x
})

#' @export
setMethod("read", "Record", function(x, con, lazy = TRUE) {
  seek(con, x@offset + 16) # Skip the header, which has already been read.

  parentRecordHeader <- header(x)

  bytes_read <- 0
  subrecords <- list()
  while(bytes_read < x@size) {
    ## Create and read subrecord
    subrecord <- Subrecord(con, seek(con), lazy, parentRecordHeader)
    subrecords <- c(subrecords, subrecord)

    ## Update tracking variables
    bytes_read <- sum(bytes_read,
                      8, # subrecord header size
                      subrecord@size)
  }

  x@subrecords <- subrecords
  names(x@subrecords) <- sapply(subrecords, \(s) s@name)

  return(x)
})

#' @export
setMethod("read", "Subrecord",
          function(x, con, lazy = TRUE, parentRecordHeader) {
            if (lazy) {
              ## Change the position of the file pointer, and do nothing else.
              seek(con, x@offset + 8 + x@size)
            } else {
              ## Seek to the start of data
              seek(con, x@offset + 8) # 8 bytes = size of header

              ## getFromNamespace doesn't have a unique error condition.
              tryCatch({
                ## Call the appropriate internal data parsing function.
                qc <- quote(parser <- getFromNamespace(sprintf("parse%sSubrecordData", x@name),
                                                       getNamespace("addamasartus")))
                eval(qc)
              },
              condition = function(e) {
                msg <- sprintf("parse%sSubrecordData is not yet implemented, or was (erroneously) not found in the addamasartus namespace!", x@name)
                msg <- sprintf("%s\nError occured while reading %s@%d",
                               msg,
                               parentRecordHeader$type,
                               parentRecordHeader$offset)
                stop(errorCondition(msg, class = "namespaceError", call = qc))
              })

              tryCatch({
                rawBytes <- readBin(con, "raw", n = x@size)
                x@data <- parser(rawBytes, parentRecordHeader)
              },
              error = function(e) {
                msg <- "An reading error occured for %s while reading %d raw bytes passed to the function. Bytes begin at %s"
                msg <- sprintf("%s\nError occured while reading %s@%d",
                               sprintf(msg, x@name, x@size, x@offset),
                               parentRecordHeader$type,
                               parentRecordHeader$offset)
                stop(msg)
              })
            }

            ## Lazy or not, return the subrecord object if no error occurred.
            return(x)
          })
