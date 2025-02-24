## Define Subrecord class first
#' @export
setClass("Subrecord",
         slots = list(
           offset = "numeric",    # file offset where subrecord begins
           
           name = "character",    # 4-byte string
           size = "numeric",      # uint32
           data = "list"
         ),
         prototype = list(
           name = "NULL",
           size = 0,
           data = list(),

           offset = 0
         ))

#' @export
Subrecord <- function(con, offset, how, parentRecordHeader) {
  seek(con, offset)
  sr_name <- rawToChar(readBin(con, "raw", n = 4))
  sr_size <- readBin(con, "integer", n = 1, size = 4, endian = "little")

  if (how == "LAZY") {
    ## Skip the data for now
    seek(con, seek(con) + sr_size)
    new("Subrecord",
        name = sr_name,
        size = sr_size,
        data = list(),
        offset = offset)
  } else {
    ## Don't skip the data, read it.
    new("Subrecord",
        name = sr_name,
        size = sr_size,
        data = list(),
        offset = offset) |>
      read(con, how, parentRecordHeader)
  }
}
