# Internal function: find matching keys and/or titles.
# Keys have to match exactly, titles approximately.
# The key takes precedence over the Title.
# INPUT
#   code            the new table code object
#   base            code for basis table, selection of rows with either 
#                   Select = TRUE or some text in code.
#   base_all_keys   all keys in the original base table
#   base_all_titles all titles in the origin base table
#' @importFrom stringdist amatch
#' @importFrom stringr str_match
#' @importFrom utils View
match_keys_and_titles <- function(code, base, base_all_keys, base_all_titles) {
  
  # 
  # try to find matching keys and titles
  #
  
  debug_match_keys_titles <- FALSE
  
  # first convert the keys and titles to lowercase.
  # for titles also remove spaced and punctuation marks.
  
  code_keys <- tolower(code$Key)
  base_keys <- tolower(base$Key)
  base_all_keys <- tolower(base_all_keys)
  
  code_titles <- code$Title
  base_titles <- base$Title
  base_all_titles <- base_all_titles
  
  if (isTRUE(all.equal(sort(code_keys), sort(base_all_keys)))) {
    
    # All keys are identical (apart from the ordering). Simply match the keys 
    # and return. Note that we compare with base_all_keys and not with 
    # base_keys, because some keys have a running number
    # (e.g. "Total_1", "Total_2"). If the number of keys is different, 
    # then we have to take care of the running numbers, this code is 
    # performed below.
    
    match_key <- match(code_keys, base_keys)
    code_rows <- which(!is.na(match_key))
    base_rows <- match_key[!is.na(match_key)]
    
    return(list(code_rows = code_rows, base_rows = base_rows))
    
  } else if (!anyDuplicated(code_titles) && !anyDuplicated(base_titles) &&
             all(base_titles %in% code_titles)) {
    
    # All selected titles have a perfect match with code_titles, which are 
    # unique. Match by title.
    
    match_title <- match(code_titles, base_titles)
    code_rows <- which(!is.na(match_title))
    base_rows <- match_title[!is.na(match_title)]
    
    return(list(code_rows = code_rows, base_rows = base_rows))
    
  } else {
    
    # 
    # More complicated case.
    #
    
    # If the keys are not equal (apart of the ordering), then check if
    # the keys contain running numbers, for example Uitvoerwaarde_1, 
    # Invoerwaarde_2, Handelsbalans_3 etc. 
    # When comparing the keys from code with base, we remove the running numbers 
    # because in the base table the corresponding key may have a different 
    # running number when another key was deleted or inserted. 
    # However, sometimes the keys without running numbers are not unique
    # (e.g. totaal_1, totaal_2 etc.), in that case we should ignore the keys.
    
    ignore_keys <- FALSE
    
    pattern <- "_(\\d+)$"
    
    has_running_numbers <- function(x) {
      ma <- str_match(x, pattern)
      suffixes <- ma[, 2]
      return(identical(suffixes, as.character(seq_along(x))))
    }
    
    remove_running_numbers <- function(x) {
      return(sub(pattern, "", x))
    }
    
    code_keys_run <- has_running_numbers(code_keys)
    base_keys_run <- has_running_numbers(base_all_keys)
    
    if (code_keys_run && base_keys_run) {
      code_keys <- remove_running_numbers(code_keys)
      base_keys <- remove_running_numbers(base_keys)
    } else  if (xor(code_keys_run, base_keys_run)) {
      
      # only one of code or base_keys have running numbers
      ignore_keys <- TRUE
    }
  }
  
  
  if (!ignore_keys) {
    match_key <- match_unique(code_keys, base_keys)
    code_rows <- which(!is.na(match_key))
    base_rows <- match_key[!is.na(match_key)]
  } else {match
    code_rows <- integer(0)
    base_rows <- integer(0)
  }
  
  if (debug_match_keys_titles) {
    cat("matching keys\n")
    print(data.frame(code = code$Key[code_rows], base = base$Key[base_rows]))
  }
  
  # now check matching titles for the rows that have no match yet
  missing_code_rows <- setdiff(seq_len(nrow(code)), code_rows)
  missing_base_rows <- setdiff(seq_len(nrow(base)), base_rows)
  
  if (debug_match_keys_titles) {
    cat("missing_code_rows\n")
    print(missing_code_rows)
  }
  
  if (length(missing_code_rows) > 0) {
    
    # no try to match titles. 
    title_matches <- match_titles(base_all_titles, code_titles)
    
    if (debug_match_keys_titles) {
      cat("base_titles\n")
      print(base_titles)
      cat("base_all_titles\n")
      print(base_all_titles)
      
      cat("code_titles\n")
      print(code_titles)
      
      cat("base_rows\n")
      print(base_rows)
      cat("code_rows\n")
      print(code_rows)
      cat("missing_base_rows\n")
      print(missing_base_rows)
      cat("missing_code_rows\n")
      print(missing_code_rows)
      
      View(cbind(base_all_titles, code_titles[title_matches]))
    }
    
    title_matches <- title_matches[match(base_titles, base_all_titles)]
    
    
    if (debug_match_keys_titles) {
      View(cbind(base_titles, code_titles[title_matches]))
    }
    
    title_matches <- title_matches[missing_base_rows]
    # remove matches with code_titles that already have been matches by key.
    title_matches[title_matches %in% code_rows] <- NA
    
    if (debug_match_keys_titles) {
      View(cbind(base_titles[missing_base_rows], code_titles[title_matches]))
      
      cat("title_matches\n")
      print(title_matches)
    }
    
    title_base_rows <- which(!is.na(title_matches))
    title_code_rows <- title_matches[!is.na(title_matches)]
    
    if (debug_match_keys_titles) {
      cat("title_base_rows(relative to missing...\n")
      print(title_base_rows)
      print(title_code_rows)
    }
    
    # title_base_rows are the row indices within missing_base_rows,
    # therefore convert to indices within respect to base
    title_base_rows <- missing_base_rows[title_base_rows]
    
    if (debug_match_keys_titles) {
      cat("title_base_rows (absolute)...\n")
      print(title_base_rows)
      print(title_code_rows)
    }
    
    if (debug_match_keys_titles) {
      cat("matching titles\n")
      print(data.frame(code = code$Title[title_code_rows], 
                       base = base$Title[title_base_rows]))
    }
    
    # title_code_rows and title_base_rows are the row numbers for the
    # row selection missing_code_rows and missing_base_rows. Now get the row 
    # number of original base and code data frames, and add to the original
    # code rows.
    code_rows <- c(code_rows, title_code_rows)
    base_rows <- c(base_rows, title_base_rows)
    
    # reorder with the order of base rows
    idx <- order(base_rows)
    code_rows <- code_rows[idx]
    base_rows <- base_rows[idx]
  }
  return(list(code_rows = code_rows, base_rows = base_rows))
}


# This function works as match, but only matches unique elements in x and y.
# TODO: is is possible to implement this more efficiently?
match_unique <- function(x, y) {
  
  # replace duplicated in y with NA  
  y_dupl <- y[duplicated(y)]
  y[y %in% y_dupl] <- NA
  
  result <- match(x, y)
  
  result_dupl <- result[duplicated(result)]
  result[result %in% result_dupl] <- NA
  
  return(result)
}

# This function works as amatch, but only matches unique elements in x and y.
# TODO: is is possible to implement this more efficiently?
amatch_unique <- function(x, y, ...) {
  
  # replace duplicated in y with NA  
  y_dupl <- y[duplicated(y)]
  y[y %in% y_dupl] <- NA
  
  result <- amatch(x, y, ...)
  
  result_dupl <- result[duplicated(result)]
  result[result %in% result_dupl] <- NA
  
  return(result)
}
