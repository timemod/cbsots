# convert a cbs period_key to the year
period_key2year <- function(period_keys) {
  return(as.integer(sub("(\\d+)(.+?)\\d+", "\\1", period_keys)))
}