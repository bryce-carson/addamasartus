parseError <- function(name) {
  msg <- sprintf("The parser parse%sSubrecordData failed during reading.", name)
  signalCondition(errorCondition(msg, class = "parseError"))
}
