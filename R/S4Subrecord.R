## Define Subrecord class first
setClass("Subrecord",
         slots = list(
           name = "character",    # 4-byte string
           size = "numeric",      # uint32
           data = "list",

           offset = "numeric"     # file offset where subrecord begins
         ),
         prototype = list(
           name = "NULL",
           size = 0,
           data = list(),

           offset = 0
         ))

Subrecord <- function(con, offset, lazy = TRUE) {
  seek(con, sr_offset <- offset)
  sr_name <- rawToChar(readBin(con, "raw", n = 4))
  sr_size <- readBin(con, "integer", n = 1, size = 4, endian = "little")

  if (lazy) {
    ## Skip the data for now
    seek(con, seek(con) + sr_size)
    new("Subrecord",
        name = sr_name,
        size = sr_size,
        data = NULL,
        offset = sr_offset)
  } else {
    new("Subrecord",
        name = sr_name,
        size = sr_size,
        data = NULL,
        offset = sr_offset) |>
      read(con)
  }
}

setMethod("read", "Subrecord",
          function(object, con, lazy = FALSE) {
            ## Seek to the start of data
            seek(con, object@offset + 8) # 8 bytes = size of header

            ## Call the appropriate internal data parsing function.
            object@data <- (
              Addamasartus:::sprintf("parse%sSubrecordData", object@name)
            )(readBin(con, "raw", n = object@size))

            object
          })
