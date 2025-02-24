## ESX class definition
#' @export
setClass("ESX",
         slots = list(path = "fs_path", # Path to the binary file
                      records = "list"), # List of Record objects
         prototype = list(path = fs::path(),
                          records = list()))

#' @export
ESX <- function(filepath, how = "TES3") {
  ## Open connection and read records
  con <- file(filepath, "rb")
  on.exit(close(con))

  read(new("ESX", path = filepath, records = list()), con = con, how)
}

## This only validates the object's file path, nothing else.
setValidity("ESX", function(object) {
  all(ifelse(c(methods::is(object@path, "fs_path"),
               fs::file_exists(object@path)),
             yes = TRUE,
             no = c(
               sprintf("@path must be an fs_path object!", is(object@path)),
               sprintf("@path does not exist (%s)!", object@path)
             )))
})
