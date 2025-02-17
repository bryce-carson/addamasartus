## ESx class definition
setClass("ESx",
         slots = list(path = "fs_path", # Path to the binary file
                      records = "list"), # List of Record objects
         prototype = list(path = fs::path(),
                          records = list()))

ESx <- function(filepath, lazy = TRUE) {
  read(new("ESx", path = filepath, records = list()), lazy = lazy)
}

## This only validates the object's file path, nothing else.
setValidity("ESx", function(object) {
  all(ifelse(c(methods::is(object@path, "fs_path"),
               fs::file_exists(object@path)),
             yes = TRUE,
             no = c(
               sprintf("@path must be an fs_path object!", is(object@path)),
               sprintf("@path does not exist (%s)!", object@path)
             )))
})
