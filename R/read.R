## NOTE: used for ESX files, Records, and Subrecords.
setGeneric("read", function(x, con, ...) standardGeneric("read"), signature = "x")

#' @param how dictactes *how* the ESX file is read. This argument is an
#'   additional parameter to the generic read method, and when reading this
#'   argument is passed onward as appropriate. "TES3" causes the reader to only
#'   parse the TES3 record (which is always parsed fully), and then return.
#'   "ENUM" will cause the entire file to be fully parsed. "LAZY" means to
#'   lazily read the subrecords (just the headers) of all records in addition to
#'   lazily reading the headers of all records.
#' @param filter a vector of record names to fully enumerate; all other record
#'   types are read lazily (only the header is read).
setMethod("read", "ESX", function(x, con, how = c("TES3", "ENUM", "LAZY"), filter = "TES3") {
  how <- match.arg(how) # signal an error if no matching argument.
  record_types <- c(
    "TES3",
    "GMST",
    "GLOB",
    "CLAS",
    "FACT",
    "RACE",
    "SOUN",
    "SKIL",
    "MGEF",
    "SCPT",
    "REGN",
    "BSGN",
    "LTEX",
    "STAT",
    "DOOR",
    "MISC",
    "WEAP",
    "CONT",
    "SPEL",
    "CREA",
    "BODY",
    "LIGH",
    "ENCH",
    "NPC_",
    "ARMO",
    "CLOT",
    "REPA",
    "ACTI",
    "APPA",
    "LOCK",
    "PROB",
    "INGR",
    "BOOK",
    "ALCH",
    "LEVI",
    "LEVC",
    "CELL",
    "LAND",
    "PGRD",
    "SNDG",
    "DIAL",
    "INFO"
  )
  filter <- match.arg(filter, record_types, several.ok = TRUE)

  ## TODO: when calling this on an existing object.
  ## Open connection and read records
  if (missing(con) && length(x@records) == 1) {
    ## Given the heuristic in this condition it's likely the user wants to
    ## finish reading the object. TODO: use a filter function parameter to
    ## filter record types to be read (lazily or enumerated).
    con <- file(x@path, "rb")
    on.exit(close(con))
  }

  bytes <- as.integer(fs::file_size(x@path))

  ## Verify magicka bytes -----
  seek(con, 0)
  maybeTES3 <- try(readBin(con, "raw", n = 4), silent = TRUE)
  if (inherits(maybeTES3, "try-error") || length(maybeTES3) < 4) {
    stop("Erorr reading file connection.")
  }

  if (!(rawToChar(maybeTES3) == "TES3")) {
    stop("Not a valid ESX file: Missing TES3 signature")
  }

  seek(con, 0)
  TES3 <- Record(con, 0, how = "ENUM", "TES3")
  HEDR <- TES3@subrecords[[1]]
  count <- HEDR@data$record_count
  if (how == "TES3") {
    records <- vector("list", length = 1)
  } else {
    records <- vector("list", length = 1 + count)
  }
  records[[1]] <- TES3

  if (how != "TES3") {
    recordIndex <- 2
    while (seek(con) < bytes) {
      ## Get current position
      current_pos <- seek(con)

      ## Fully read records that are part of a given set, otherwise read lazily.
      if (how == "ENUM" && !missing(filter)) {
        records[[recordIndex]] <- Record(con, current_pos, how, filter)
      } else if (!missing(filter)) {
        simpleWarning("`how` is \"LAZY\" but `filter` was supplied; filter
 functions do nothing when reading lazily, so the filter is ignored.")
        records[[recordIndex]] <- Record(con, current_pos, how, record_types[-1])
      }

      recordIndex <- recordIndex + 1
    }
  }

  x@records <- records

  return(x)
})

setMethod("read", "Record", function(x, con, how = c("LAZY", "ENUM")) {
  how <- match.arg(how)

  ## `Record()` calls the `read` method for Record classed objects (all helper
  ## functions call the reader.
  seek(con, x@offset + 16)

  subrecordBytesRead <- 0
  subrecords <- list()
  while (subrecordBytesRead < x@size) {
    ## Create and read subrecord
    subrecord <- Subrecord(con, seek(con), how, header(x))
    subrecords <- c(subrecords, subrecord)

    ## Update tracking variables
    subrecordBytesRead <- sum(subrecordBytesRead,
                              4, # NAME
                              4, # SIZE
                              subrecord@size)
  }

  x@subrecords <- subrecords
  names(x@subrecords) <- sapply(subrecords, \(s) s@name)

  invisible(x)
})

setMethod("read", "Subrecord", function(x, con, how = c("LAZY", "ENUM"), RecordHeader) {
  how <- match.arg(how)

  if (how == "LAZY") {
    ## Change the position of the file pointer, and do nothing else.
    seek(con, x@offset + 8 + x@size)
    invisible(x)
  }

  ## Seek to the start of data
  seek(con, x@offset + 8) # 8 bytes = size of header

  ## getFromNamespace doesn't have a unique error condition.
  tryCatch({
    ## Call the appropriate internal data parsing function.
    parserName <- sprintf("parse%sSubrecordData", x@name)
    ns <- getNamespace("addamasartus")
    qc <- substitute(parser <- getFromNamespace(parserName, ns))
    eval(qc)
  },
  condition = function(e) {
    fmt <- "parse%sSubrecordData is not yet implemented, or was (erroneously)
not found in the addamasartus namespace! Error occured while reading %s@%d."
    msg <- sprintf(fmt, x@name, x@name, x@offset)
    stop(errorCondition(msg, class = "namespaceError", call = qc))
  })

  ## Read the binary data of the subrecord and parse it according to x@name.
  tryCatch({
    x@data <- parser(con, x, RecordHeader)
  },
  error = function(e) {
    fmt <- "An error (of class %s) occurred while reading or parsing %s (0x%02X; within %s 0x%02X):
  %s"
    stop(sprintf(fmt,
                 class(e),
                 x@name,
                 x@offset,
                 RecordHeader$type,
                 RecordHeader$offset,
                 conditionMessage(e)))
  })

  ## Lazy or not, return the subrecord object if no error occurred.
  invisible(x)
})
