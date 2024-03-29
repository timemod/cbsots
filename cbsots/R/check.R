# check if the table is a dutch table
check_language <- function(meta) {
  if (meta$TableInfos$Language != "nl") {
    stop("Package cbsots can currently only handle dutch tables")
  }
  return()
}

# This function checks if the timeseries code is valid. For example, it checks 
# if the code contains duplicate codes, and compares the keys and titles
# with the keys and titles of the CBS coding.
#
# ARGUMENTS:
#  id             table id
#  code           timeseries coding created with edit_ts_code
#  selected_code  same as code, except that only rows with Select = TRUE
#                 are included
#  cbs_code       the Keys and Titles for the original CBS table
#  downloaded     a logical: TRUE if cbs_code has just been downloaded
#                 from the CBS website. If FALSE, then cbs_code has
#                 been read from a file. This argument determines the
#                 error messages and warning.
#
check_code <- function(id, code, selected_code, cbs_code, downloaded) {
  
  if (!identical(sort(names(code)), sort(names(cbs_code)))) {
    stop("Corrupt timeseriescode: dimension names does not agree with CBS names")
  }
  
  check <- function(name) {
    
    tscode <- code[[name]]
    selected_tscode <- selected_code[[name]]
    cbs <- cbs_code[[name]]
    
    # check that all non-empty codes are unique
    codes <- selected_tscode$Code
    codes <- codes[nchar(codes) > 0]
    if (anyDuplicated(codes)) {
      duplicates <- unique(codes[duplicated(codes)])
      stop(paste0("Duplicate codes found for ",  name, ":\n", 
                  paste(duplicates, collapse = "\n")), "\n.")
    }
    
    # at least one key should be selected
    if (!any(tscode$Select)) {
      stop(paste0("No single key selected for ", name, "."))
    }
    
    # check for duplicate Keys in selected_tscode 
    # (this should be an error)
    if (anyDuplicated(selected_tscode$Key)) {
      dupl_keys <- cbs$Key[duplicated(selected_tscode$Key)]
      dupl_keys <- paste0("'", dupl_keys, "'")
      stop("Duplicate keys selected in timeseries coding for dimension ", name, 
              " in table ", id,  ":\n", paste(dupl_keys, collapse = ", "),
              ".")
    }
    
    # check for duplicate cbs Keys (this is just a warning)
    if (anyDuplicated(cbs$Key)) {
      dupl_keys <- cbs$Key[duplicated(cbs$Key)]
      dupl_keys <- paste0("'", dupl_keys, "'")
      warning("Duplicate keys in cbs meta data for dimension ", name, 
             " in table ", id,  ":\n", paste(dupl_keys, collapse = ", "),
               ".")
    }
    
    # check if the keys are different
    if (nrow(tscode) != nrow(cbs) || 
        !identical(sort(tscode$Key), sort(cbs$Key))) {
      
      # Keys are different
      
      if (downloaded) {
        key_source <- "CBS keys"
        advice <-  paste("Update the table coding with the shiny application",
                         "edit_ts_code.")
      } else {
        key_source <- "the keys on file"
        advice <- paste("Download the data with function get_ts using",
                        "argument refresh or download.")
      }
      
      # Find problematic keys with a running number
      # If the keys without running numbers are not unique,
      # and the keys are not identical, it could be case that
      # a key corresponds to a different variables, so we should
      # give an error in that case, which forces the user 
      # to update the table.
      missing_keys_code <- setdiff(cbs$Key, tscode$Key)
      missing_keys_cbs  <- setdiff(tscode$Key, cbs$Key)
      problem_keys <- union(missing_keys_code, missing_keys_cbs)
      problem_keys <- grep("_\\d+$", problem_keys, value = TRUE)
      if (length(problem_keys) > 0) {
        problem_keys_no_run <- sub("_\\d+$", "", problem_keys)
        code_keys_no_run <- sub("_\\d+$", "", tscode$Key)
        cbs_keys_no_run <- sub("_\\d+$", "", cbs$Key)
        dupl_code <- unique(code_keys_no_run[duplicated(code_keys_no_run)])
        dupl_cbs <- unique(code_keys_no_run[duplicated(code_keys_no_run)])
        dupl <- union(dupl_code, dupl_cbs)
        if (length(intersect(dupl, problem_keys_no_run)) > 0) {
          stop(paste0("Keys in code for dimension ", name, " in table ", id,  
                      " do not agree with ", key_source, ".\n",
                      "The problem keys without running number are not unique.\n",
                      advice))
        }    
      }
      
      warning(paste0("Keys in code for dimension ", name, " in table ", id,  
                     " do not agree with ", key_source, ".\n",
                     advice))
      
    } else {
      
      # The keys are identical. Check if the titles have changed.
      # Since the Keys are identical, we can order the titles using the
      # alphabetic ordering of the keys each data frame.
      code_titles <- tscode$Title[order(tscode$Key)]
      cbs_titles <- cbs$Title[order(cbs$Key)]
    
      if (downloaded) {
        key_source <- "CBS keys"
        advice <-  paste("Update the table coding with the shiny application",
                         "edit_ts_code.")
      } else {
        key_source <- "the keys on file"
        advice <- paste("Download the data with function get_ts using",
                        "argument refresh or download.")
      }
      
      if (!identical(code_titles, cbs_titles)) {
        
        warning(paste0("Titles in code for dimension ", name, " in table ", id,  
                       " do not agree with ", key_source, ".\n",
                       advice))
      }
    }
    
    return()
  }
  
  dum <- lapply(names(code), FUN = check)
  
  return()
}


# This function checks for keys in code that are not in cbs_code
check_unknown_keys <- function(id, code, cbs_code) {
  
  check <- function(name) {
    
    tscode <- code[[name]]
    cbs <- cbs_code[[name]]
    
    # Check for unknown selected keys => Error
    unknown_keys <- setdiff(tscode$Key, cbs$Key)
    if (length(unknown_keys) > 0) {
      stop(paste0("Unknown keys in code for dimension ", name, 
                  " in table ", id, ":\n", 
                  paste(unknown_keys, collapse = "\n")), "\n.")
    }
    return()
  }
  
  
  dum <- lapply(names(code), FUN = check)
  
  return()
}


