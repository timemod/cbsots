# Parses the CBS period keys. Returns a data frame with columns key, year, 
# freq_cbs and freq
parse_period_keys <- function(period_keys, warn_unknown_freqs = FALSE) {
  period_pattern <- "^(\\d+)([a-zA-Z]+)(\\d*)$"
  period_key_info <- stringr::str_match(period_keys, period_pattern)
  period_key_info <- as.data.frame(period_key_info)[, -4]
  colnames(period_key_info) <- c("key", "year", "freq_cbs")
  period_key_info$year <- as.numeric(period_key_info$year)
  if (warn_unknown_freqs) {
    unknown_freqs <- setdiff(unique(period_key_info$freq_cbs), 
                             freq_table$freq_cbs)
    if (length(unknown_freqs) > 0) {
      warning("Unknown frequencies ", paste(unknown_freqs, collapse = ", "), 
              " in CBS data")
    }
  }
  period_key_info <- merge(period_key_info, freq_table, by = "freq_cbs")
  return(period_key_info)
}

# Check specified frequencies. Used in functions download.table and read.table.
# Returns the frequencies with the frequencies not found in the CBS data data 
# removed.
check_frequencies <- function(frequencies, period_key_info) {
  available_freqs <- unique(period_key_info$freq)
  missing_freqs <- setdiff(frequencies, available_freqs)
  if (length(missing_freqs) > 0) {
    warning(paste(
      "Frequencies", paste(missing_freqs, collapse = ", "),
      "not present in CBS data"
    ))
    frequencies <- intersect(frequencies, available_freqs)
  }
  if (length(frequencies) == 0) {
    stop("None of the requested frequencies is present in the CBS data")
  }
  return(frequencies)
}

# Check min_year. Used in functions download.table and read.table.
check_min_year <- function(min_year, period_key_info) {
  available_years <- as.numeric(unique(period_key_info$year))
  if (max(available_years) < min_year) {
    stop("There is no data available for years >= ", min_year, 
         ".\nThe last year with data is ", max(available_years), ".")
  }
  return(invisible())
}


order_code_rows <- function(code,  cbs_order) {

  orig_key_order <- code$OrigKeyOrder
  
  if (cbs_order) {
    
    required_order <- orig_key_order
    
  } else {
    
    # selected rows first
    selected <- code$Key[code$Select]
    not_selected <- code$Key[!code$Select]
    selected_ordered <- selected[match(intersect(orig_key_order, selected),
                                       selected)]
    not_selected_ordered <- not_selected[match(intersect(orig_key_order, 
                                                         not_selected), 
                                               not_selected)]
    required_order <- c(selected_ordered, not_selected_ordered) 
  }
  
  order <-  match(required_order, code$Key)
  
  code[ , 1:4] <- code[order, 1:4]
  
  return(code)
}
