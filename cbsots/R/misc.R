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