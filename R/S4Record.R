## Modified Record class to contain list of Subrecords
setClass("Record",
         slots = list(
           name = "character",    # 4-byte string
           size = "numeric",      # uint32
           flag1 = "numeric",     # uint32
           flag2 = "numeric",     # uint32
           subrecords = "list",   # list of Subrecord objects

           offset = "numeric"     # file offset where record begins
         ),
         prototype = list(
           name = "TES3",
           size = 300,
           flag1 = 0,
           flag2 = 0,
           subrecords = list(),

           offset = 0
         ))

Record <- function(con, offset, ...) {
  ## Seek to the record start
  seek(con, offset)

  record_name <- rawToChar(readBin(con, "raw", n = 4))
  record_size <- readBin(con, "integer", n = 1, size = 4, endian = "little")
  record_flag1 <- readBin(con, "integer", n = 1, size = 4, endian = "little")
  record_flag2 <- readBin(con, "integer", n = 1, size = 4, endian = "little")

  r <- new("Record",
           name = record_name,
           size = record_size,
           flag1 = record_flag1,
           flag2 = record_flag2,
           subrecords = list(),
           offset = offset)

  ## If the argument is provided use whatever value was provided, despite
  ## defaults assuring laziness.
  if ("lazy" %in% ...names()) {
    lazy <- list(...)$lazy
    read(r, con = con, lazy)
  } else {
    ## Skip the subrecord data for now
    seek(con, seek(con) + record_size)
    read(r, con = con)
  }
}
