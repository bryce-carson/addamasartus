## Define Subrecord class first
setClass("Subrecord",
         slots = list(
           ## test
           name = "character",    # 4-byte string
           size = "numeric",      # uint32
           offset = "numeric",    # file offset where subrecord begins
           data = "raw"           # raw data content
         ),
         prototype = list(
           name = "",
           size = 0,
           offset = 0,
           data = raw(0)
         ))

## Initialize method for Subrecord
setMethod("initialize", "Subrecord",
          function(.Object, con, offset, ...) {
            .Object@offset <- offset

            ## Seek to the subrecord start
            seek(con, offset)

            ## Read the 4-character name
            name_raw <- readBin(con, "raw", n = 4)
            .Object@name <- rawToChar(name_raw)

            ## Read the size uint32
            .Object@size <- readBin(con, "integer", n = 1, size = 4, endian = "little")

            ## Skip the data for now
            seek(con, seek(con) + .Object@size)

            .Object
          })

## Method to read subrecord data
setGeneric("readSubrecord", function(object, con) standardGeneric("readSubrecord"))

setMethod("readSubrecord", "Subrecord",
          function(object, con) {
            ## Seek to the start of data
            seek(con, object@offset + 8) # 8 bytes = size of header

            ## Read the raw data
            object@data <- readBin(con, "raw", n = object@size)

            object
          })
