## NOTE: used for ESx files, Records, and Subrecords.
#' @export
setGeneric("read", function(x, con, lazy = TRUE) standardGeneric("read"), signature = "x")

#' @export
setMethod("read", "ESx", function(x, con, lazy = TRUE) {
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
        stop("Bytes (insufficient for even a record header) remain near EOF, but was unable to read them.")
    }

    ## If reading of the connection has just begun, ensure that the last four
    ## bytes read (a record header name) are the magic bytes for this file
    ## type.
    if (seek(con) == 4 &&
        !(is_tes3_record <- rawToChar(test_read) == "TES3")) {
      stop("Not a valid ESx file: Missing TES3 signature")
    } else if (is_tes3_record) {
      TES3 <- Record(con, current_pos, lazy = FALSE)
      HEDR <- TES3@subrecords[[1]]
      count <- HEDR@data$record_count

      if (is.null(count) || !is.numeric(count) || 0 > count)
        stop("Critical error obtaining record count from HEDR subrecord of TES3 record.")

      records <- vector("list", length = 1 + count)
    } else {
      ## Seek back to start of record (because we read its header),
      seek(con, current_pos)

      ## and create new records lazily.
      records[which(sapply(records, is.null))] <- Record(con, current_pos)
    }
  }

  x@records <- records

  x
})

#' @export
setMethod("read", "Record", function(x, con, lazy = TRUE) {
  seek(con, x@offset + 16) # Skip the header, which has already been read.

  bytes_read <- 0
  subrecords <- list()
  while(bytes_read < x@size) {
    ## Create and read subrecord
    subrecord <- Subrecord(con, seek(con), lazy)
    subrecords <- c(subrecords, subrecord)

    ## Update tracking variables
    bytes_read <- sum(bytes_read,
                      8, # subrecord header size
                      subrecord@size)
  }

  x@subrecords <- subrecords

  return(x)
})

#' @export
setMethod("read", "Subrecord",
          function(x, con, lazy = TRUE) {
            if (lazy) {
              ## Change the position of the file pointer, and do nothing else.
              seek(con, x@offset + 8 + x@size)
            } else {
              ## Seek to the start of data
              seek(con, x@offset + 8) # 8 bytes = size of header

              tryCatch({
                ## Call the appropriate internal data parsing function.
                parser <- getFromNamespace(sprintf("parse%sSubrecordData", x@name),
                                           getNamespace("addamasartus"))
              },
              error = function(e) {
                msg <- sprintf("parse%sSubrecordData is not yet implemented, or was (erroneously) not found in the addamasartus namespace!", x@name)
                signalCondition(errorCondition(msg, class = "namespaceError"))
              })

              tryCatch({
                rawBytes <- readBin(con, "raw", n = x@size)
                x@data <- parser(rawBytes)
              },
              parseError = function(e) {
                signalCondition(e)
              },
              error = function(e) {
                "An reading error occured for %s while reading %d raw bytes passed to the function." |>
                  sprintf(x@name, x@size) |>
                  stop()
              })
            }

            ## Lazy or not, return the subrecord object if no error occurred.
            return(x)
          })
