# This function selects the period keys with the specified frequencies and with 
# year >= min_year.  The function is used when the user has supplied argument 
# 'frequencies' or 'min_year' of function get_ts.
get_period_keys <- function(meta, min_year, frequencies, 
                            warnings = TRUE) {
  
  period_keys <- meta$Perioden$Key

  freq_table <- c(JJ = "Y", HJ = "H", KW = "Q", MM = "M")
  periode_pattern <- "^(\\d+)([a-zA-Z]+)(\\d+)$"

  period_keys <-  grep(periode_pattern, period_keys, value = TRUE)
  
  period_key_freqs <- sub(periode_pattern, "\\2", period_keys)

  # If frequencies not specified, give a warning about unknown
  # frequencies in CBS meta data.
  if (is.null(frequencies)) {
    unknown_freqs <- setdiff(unique(period_key_freqs), names(freq_table)) 
    if (warnings && length(unknown_freqs) > 0) {
      warning("Unknown frequencies ", paste(unknown_freqs, collapse = ", "), 
              " in CBS data")
    }
  }
  
  sel <- period_key_freqs %in% names(freq_table)
  period_keys <- period_keys[sel]
  period_key_freqs <- period_key_freqs[sel]
  
  # check frequencies
  if (!is.null(frequencies)) {
    freqs <- freq_table[period_key_freqs]
    period_keys <- period_keys[freqs %in% frequencies]
    freqs_present <- unique(freqs)
    missing_frequencies <- setdiff(frequencies, freqs_present)
    if (warnings && length(missing_frequencies) > 0) {
      warning(paste("Frequencies", paste(missing_frequencies, collapse = ", "),
                    "not present in CBS data"))
    }
    if (length(period_keys) == 0) {
      stop("None of the requested frequencies is present in the CBS data")
    }
  }
  
  if (!is.null(min_year)) {
    years <- as.integer(sub(periode_pattern, "\\1", period_keys))
    period_keys <- period_keys[years >= min_year]
    if (length(period_keys) == 0) {
      stop("There is no data available for years >= ", min_year, 
           ".\nThe last year with data is ", max(years), ".")
    }
  }
  
  return(period_keys)
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
