# check if the table is a dutch table
check_language <- function(meta) {
  if (meta$TableInfos$Language != "nl") {
    stop("Function get_ts can currently only handle dutch tables")
  }
  return()
}

# This function checks if the code contains duplicate codes and compares it with
# the cbs code. It checks for unknown Keys (Keys that are not present in the 
# meta data). It also updates the Titles with the CBS titles, because
# the titles may be out of date.
check_code <- function(code, cbs_code) {

  if (!identical(sort(names(code)), sort(names(cbs_code)))) {
    stop("Corrupt timeseriescode: dimension names does not agree with CBS names")
  }
  
  convert <- function(name) {
    
    tscode <- code[[name]]
    cbs <- cbs_code[[name]]
    
    # check all non-empty codes
    codes <- tscode$Code
    codes <- codes[nchar(codes) > 0]
    if (anyDuplicated(codes)) {
      duplicates <- unique(codes[duplicated(codes)])
      stop(paste0("Duplicate codes found for ",  name, ":\n", 
                  paste(duplicates, collapse = "\n")), "\n.")
    }
    
    unknown_keys <- setdiff(tscode$Key, cbs$Key)
    if (length(unknown_keys) > 0) {
      stop(paste0("Unknown keys in code for ", name, ":\n", 
                paste(unknown_keys, collapse = "\n")), "\n.")
    }
    
    # at least one key should be selected
    if (!any(tscode$Select)) {
      stop(paste0("No single key selected for ", name, "."))
    }
    
    # TODO: update Titles in code based on cbs_code, then check_code
    # should return a code with updated Titles
  
    return(tscode)
  }
  
  return(sapply(names(code), FUN = convert, simplify = FALSE))
}

# this function returns TRUE if data contains all keys in code and period_keys
check_read_data <- function(data, code, period_keys) {
  
  # check dimension keys
  for (dimension in setdiff(names(code), "Topic")) {
    if (any(!code[[dimension]]$Key %in% data[[dimension]])) {
      return(FALSE)
    }
  }
  return(all(period_keys %in% data$Perioden))
}