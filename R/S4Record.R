## Modified Record class to contain list of Subrecords
setClass("Record",
         slots = list(
           name = "character",    # 4-byte string
           size = "numeric",      # uint32
           flag1 = "numeric",     # uint32
           flag2 = "numeric",     # uint32
           offset = "numeric",    # file offset where record begins
           subrecords = "list"    # list of Subrecord objects
         ),
         prototype = list(
           name = "",
           size = 0,
           flag1 = 0,
           flag2 = 0,
           offset = 0,
           subrecords = list()
         ))

## Initialize method for Record
setMethod("initialize", "Record",
          function(.Object, con, offset, ...) {
            .Object@offset <- offset

            ## Seek to the record start
            seek(con, offset)

            ## Read the 4-character name
            name_raw <- readBin(con, "raw", n = 4)
            .Object@name <- rawToChar(name_raw)

            ## Read the uint32 values
            .Object@size <- readBin(con, "integer", n = 1, size = 4, endian = "little")
            .Object@flag1 <- readBin(con, "integer", n = 1, size = 4, endian = "little")
            .Object@flag2 <- readBin(con, "integer", n = 1, size = 4, endian = "little")

            ## Skip the subrecord data for now
            seek(con, seek(con) + .Object@size)

            .Object
          })

## Method to read all subrecords in a record
setGeneric("readRecord", function(object, con) standardGeneric("readRecord"))

setMethod("readRecord", "Record",
          function(object, con) {
            ## Seek to the start of subrecord data
            current_offset <- object@offset + 16 # 16 bytes = size of header
            seek(con, current_offset)

            ## Read subrecords until we've consumed size bytes
            bytes_read <- 0
            subrecords <- list()

            while(bytes_read < object@size) {
              ## Create and read subrecord
              subrecord <- new("Subrecord", con, current_offset)
              subrecord <- readSubrecord(subrecord, con)

              ## Add to list
              subrecords <- c(subrecords, list(subrecord))

              ## Update tracking variables
              bytes_read <- bytes_read + 8 + subrecord@size # header + data
              current_offset <- current_offset + 8 + subrecord@size
            }

            object@subrecords <- subrecords
            object
          })

## Example usage:
## con <- file("example.bin", "rb")
## record <- new("Record", con, offset = 0)
## record_with_subrecords <- readRecord(record, con)
## close(con)
