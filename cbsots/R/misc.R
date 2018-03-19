# returns period keys for all periods with year >= min_year
get_period_keys <- function(meta, min_year) {
  period_keys <- meta$Perioden$Key
  if (is.null(min_year)) {
    return(period_keys)
  } else {
    years <- as.integer(sub("(\\d+)(.+?)\\d+", "\\1", period_keys))
    return(period_keys[years >= min_year])
  }
}