## NOTE: used for ESx files, Records, and Subrecords.
setGeneric("read", function(x, ..., lazy = TRUE) standardGeneric("read"), signature = "x")

setMethod("read", "ESx", function(x, ..., lazy = TRUE) {
  ## Open connection and read records
  con <- file(x@path, "rb")
  on.exit(close(con))

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
      record <- Record(con, current_pos, lazy = FALSE)

      ## TODO: remove this after things are working nicely.
      str(record)
      assign("TES3", record, envir = .GlobalEnv)

      HEDR <- record@subrecords[[1]]
      ## FIXME: the data is empty! It probably isn't getting parsed!
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

setMethod("read", "Record", function(x, ..., lazy = TRUE) {
  if ("con" %in% ...names()) {
    con <- list(...)$con
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

    x
  } else {
    stop("The read method for Record requires the `con` argument supply a connection object.")
  }
})

setMethod("read", "Subrecord",
          function(x, ..., lazy = TRUE) {
            if ("con" %in% ...names()) {
              if (lazy) {
                seek(con, x@offset + 8 + x@size)
              } else {
                ## Seek to the start of data
                seek(con, x@offset + 8) # 8 bytes = size of header

                ## Handle the case that no parser is defined for this subrecord
                ## type by asking the user to define one interactively or fail.
                tryCatch({
                  parser <- addamasartus:::sprintf("parse%sSubrecordData", x@name)
                },
                signalCondition(simpleCondition(sprintf("No parser `addamasartus:::parse%sSubrecordData` was found.", x@name))),
                \(c) signalCondition(c))

                ## Call the appropriate internal data parsing function.
                x@data <- parser(readBin(con, "raw", n = x@size))
              }

              x
            } else {
              stop("The read method for Subrecord requires the `con` argument supply a connection object.")
            }
          })
