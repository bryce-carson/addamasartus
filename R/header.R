## NOTE: used for ESx files, Records, and Subrecords.
setGeneric("header", function(x) standardGeneric("header"))

setMethod("header", "Record", function(x) {
  return(list(type = x@name,
              size = x@size,
              flag1 = x@flag1,
              flag2 = x@flag2,

              offset = x@offset))
})

setMethod("header", "Subrecord", function(x) {
  return(list(type = x@name,
              size = x@size,
              offset = x@offset))
})
