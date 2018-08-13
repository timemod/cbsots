# Internal function: fill in Select and Code from an old table into table.
# Use common dimension names and Keys or Titles.
update_table <- function(table, old_table) {

  
  update_code <- function(dimension) {
    
    code <- table$codes[[dimension]]
    base_code <- old_table$codes[[dimension]]
    
    # Find matching keys and/or titles, and update code based on base_code.

    ret <- match_keys_and_titles(code, base_code)
    
    base_code_select <- base_code[ret$base_rows]$Select
    code_select <- code[ret$code_rows]$Select
    base_code_code <- base_code[ret$base_rows]$Code
    code_code <- code[ret$code_rows]$Code
    
    code[ret$code_rows, "Select"] <- ifelse(base_code_select, base_code_select, 
                                            code_select)
    code[ret$code_rows, "Code"] <- ifelse(is.na(base_code_code) | 
                                            base_code_code == "", 
                                          code_code, base_code_code)

    # Check if all previously selected variables have been found
    orig_selected <- which(base_code$Select | 
                          !(is.na(base_code$Code) | base_code$Code == "")) 
    
    if (length(ret$base_rows) == 0) {
      warning(paste0("No matching entries found for dimension ", dimension, 
                     "."))
    } else {
      missing <- setdiff(orig_selected, ret$base_rows)
      if (length(missing) > 0) {
        warning(paste0("No matching entries found for dimension ", dimension,
                       ":\n",
                       paste(paste(base_code$Key[missing], 
                                   paste0("\"", base_code$Title[missing], "\""), 
                                   sep = " - "),
                             collapse = "\n")))
      }
    }
    
    # Reorder the rows of code if base_code was ordered according to selected 
    # first.
    cbs_order <- identical(base_code$Key, base_code$OrigKeyOrder)
    if (!cbs_order) {
      code <- order_code_rows(code, cbs_order)
    }
    
    return(code)
  }
  
  common_dimensions <- intersect(names(table$codes), names(old_table$codes))
  new_codes <- sapply(common_dimensions, FUN = update_code, simplify = FALSE)
  table$codes <- modifyList(table$codes, new_codes)
  
  # also adapt the ordering of the dimensions
  ord <- match(old_table$order, table$order)
  ord <- ord[!is.na(ord)]
  ord <- c(ord, setdiff(seq_along(table$order), ord))
  table$order <- table$order[ord]
  
  # update cbs_key_order
  table$cbs_key_order[common_dimensions] <- 
                   old_table$cbs_key_order[common_dimensions]
  
  return(table)
}

# Internal function: find matching keys and/or titles.
# Keys have to match exactly, titles approximately.
# The key takes precedence over the Title.
#' @importFrom stringdist amatch
#' @importFrom stringr str_match
match_keys_and_titles <- function(code, base) {
  
  # 
  # check keys
  #
  
  debug_match_keys_titles <- FALSE

  code_keys <- tolower(code$Key)
  base_keys <- tolower(base$Key)
  ignore_keys <- FALSE
  
  if (!isTRUE(all.equal(code_keys, base_keys))) {

    # If the keys are not equal (apart of the ordering), then check if
    # the keys contain running numbers, for example Uitvoerwaarde_1, 
    # Invoerwaarde_2, Handelsbalans_3 etc. 
    # When comparing the keys from code with base, we remove the running numbers 
    # because in the base table the corresponding key may have a different 
    # running number when another key was deleted or inserted. 
    # However, sometimes the keys without running numbers are not unique
    # (e.g. totaal_1, totaal_2 etc.), in that case we should ignore the keys.
    
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
    base_keys_run <- has_running_numbers(base_keys)
    
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
  
  convert_char <- function(x) {
    x <- tolower(x)
    return(gsub("[  ,;.:]", "", x))
  }
  
  if (debug_match_keys_titles) {
    cat("missing_code_rows\n")
    print(missing_code_rows)
  }
  
  if (length(missing_code_rows) > 0) {
    # Check if the titles are the same, ignoring spaces and some
    # punctuation characters.
    code_titles <- convert_char(code$Title[missing_code_rows])
    base_titles <- convert_char(base$Title[missing_base_rows])
    
    if (anyDuplicated(code_titles) || anyDuplicated(base_titles)) {
      return(list(code_rows = integer(0), base_rows = integer(0)))
    }
    
    # TODO: use amatch only to match unique pairs, see function
    #       match_unique.
    
    match_title <- amatch(code_titles, base_titles, method = "jw", 
                          maxDist = 0.2)
    title_code_rows <- which(!is.na(match_title))
    title_base_rows <- match_title[!is.na(match_title)]
    
    if (debug_match_keys_titles) {
      cat("matching titles\n")
      print(data.frame(code = code$Title[title_code_rows], 
                     base = base$Title[title_base_rows]))
    }
    
    # title_code_rows and title_base_rows are the row numbers for the
    # row selection missing_code_rows and missing_base_rows. Now get the row 
    # number of original base and code data frames, and add to the original
    # code rows.
    code_rows <- c(code_rows, missing_code_rows[title_code_rows])
    base_rows <- c(base_rows, missing_base_rows[title_base_rows])
  }
  return(list(code_rows = code_rows, base_rows = base_rows))
}


# This function works as match, but only matches unique elements in x and y.
# TODO: is is possible to implement this more efficiently
match_unique <- function(x, y) {
  
  # replace duplicated in y with NA  
  y_dupl <- y[duplicated(y)]
  y[y %in% y_dupl] <- NA
  
  result <- match(x, y)
  
  result_dupl <- result[duplicated(result)]
  result[result %in% result_dupl] <- NA
  
  return(result)
}

# TODO: create similar function amatch_unique