# returns period keys for all periods with year >= min_year
get_period_keys <- function(meta, min_year, frequencies) {
  
  period_keys <- meta$Perioden$Key

  freq_table <- c(JJ = "Y", KW = "Q", MM = "M")
  
  # check frequencies
  if (!is.null(frequencies)) {
    freqs_cbs <- sub("\\d+(.+?)\\d+", "\\1", period_keys)
    freqs <- freq_table[freqs_cbs]
    period_keys <- period_keys[freqs %in% frequencies]
    freqs_present <- unique(freqs)
    missing_frequencies <- setdiff(frequencies, freqs_present)
    if (length(missing_frequencies) > 0) {
      warning(paste("Frequencies", paste(missing_frequencies, collapse = ", "),
                    "not present in table"))
    }
  }
  
  if (is.null(min_year)) {
    return(period_keys)
  } else {
    years <- as.integer(sub("(\\d+)(.+?)\\d+", "\\1", period_keys))
    return(period_keys[years >= min_year])
  }
}