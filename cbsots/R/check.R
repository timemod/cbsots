# check if the table is a dutch table
check_language <- function(meta) {
  if (meta$TableInfos$Language != "nl") {
    stop("Function get_ts can currently only handle dutch tables")
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
    codes <- tscode$Code
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
    
    # check if the keys have changed
    if (nrow(tscode) != nrow(cbs) || 
        !identical(sort(tscode$Key), sort(cbs$Key))) {
      
      # Keys are different
      
      # Check for unknown selected keys => Error
      unknown_keys <- setdiff(selected_tscode$Key, cbs$Key)
      if (length(unknown_keys) > 0) {
        stop(paste0("Unknown keys in code for dimension ", name, 
                    " in table ", id, ":\n", 
                  paste(unknown_keys, collapse = "\n")), "\n.")
      
      } else {
        
        if (downloaded) {
          key_source <- "CBS keys"
          advice <-  paste("Update the table coding with the shiny application",
                           "edit_ts_code.")
        } else {
          key_source <- "the keys on file"
          advice <- paste("Download the data with function get_ts using",
                          "argument refresh or download.")
        }
      
        # find problematic keys with a running number
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
      }
      
    } else {
      
     
      
      # The keys are identical. Maybe the titles have changed
      code_titles <- tscode$Title
      cbs_titles <- cbs$Title
      code_titles <- code_titles[match(cbs$Key, tscode$Key)]
      
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


