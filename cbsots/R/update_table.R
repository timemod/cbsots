# Internal function: fill in Select and Code from an old table into table.
# Use common dimension names and Keys or Titles.
update_table <- function(table, old_table, table_id, old_table_id) {

  update_code <- function(dimension) {
    
    code <- table$codes[[dimension]]
    base <- old_table$codes[[dimension]]
    
    # check key ordering
    cbs_order <- identical(base$Key, base$OrigKeyOrder)
    
    # save all keys and titles of the base table
    base_all_keys <- base$Key
    base_all_titles <- base$Title
    
    # now only select rows in base with either Select = TRUE or some text in 
    # Code
    base <- base[base$Select | !(is.na(base$Code) | base$Code == ""), , 
                 drop = FALSE]
    
    # Find matching keys and/or titles, and update code based on base_code.
    matches <- match_keys_and_titles(code, base, base_all_keys, base_all_titles)
    
    base_select <- base[matches$base_rows]$Select
    code_select <- code[matches$code_rows]$Select
    base_code   <- base[matches$base_rows]$Code
    code_code   <- code[matches$code_rows]$Code
    
    code[matches$code_rows, "Select"] <- ifelse(base_select, base_select, 
                                                code_select)
    code[matches$code_rows, "Code"] <- ifelse(is.na(base_code) | base_code == "", 
                                              code_code, base_code)

    check_matches(matches, code, base, dimension)
    
    if (!cbs_order) {
      # Reorder the rows of code if base_code was ordered according to selected 
      # first.
      code <- order_code_rows(code, cbs_order)
    }
    
    return(code)
  }
  
  
  check_matches <- function(matches, code, base, dimension) {
    
    # Check if variables in base_code with either Select = TRUE or a specified
    # code have been matched.
    
    # check missing elements  
    missing <- setdiff(seq_len(nrow(base)), matches$base_rows)
    
    if (nrow(base) > 0 && length(matches$base_rows) == 0) {
      warning(paste0("No matching entries found for dimension ", dimension, 
                     "."))
    } else {
      if (length(missing) > 0) {
        warning(paste0("No matching entries found for dimension ", dimension,
                       ":\n",
                       paste(paste(base$Key[missing], 
                                   paste0("\"", base$Title[missing], "\""), 
                                   sep = " - "),
                             collapse = "\n")))
      }
    }
    
    match_dir <- "match_reports"
    if (!dir.exists(match_dir)) {
      dir.create(match_dir)
    }
    match_file <- file.path(match_dir, paste0(old_table_id, "_", 
                                              table_id, "_", dimension, 
                                              ".xlsx"))
    
    ma <- data.frame(base_key = base$Key[matches$base_rows],
                     code_key = code$Key[matches$code_rows],
                     base_title = base$Title[matches$base_rows],
                     code_title = code$Title[matches$code_rows],
                     stringsAsFactors = FALSE)
    
    is_perfect <- ma$base_key == ma$code_key & ma$base_title == ma$code_title
    perfect_match <- ma[is_perfect, ]
    imperfect_match <- ma[!is_perfect, ]

    
 

    no_match <- data.frame(base_key = base$Key[missing], 
                           base_title = base$Title[missing],
                           stringsAsFactors = FALSE)
     
    wb <- createWorkbook()
    sheet <- createSheet(wb, "perfect match")
    if (nrow(perfect_match) > 0) {
      addDataFrame(perfect_match, sheet, row.names = FALSE)
      autoSizeColumn(sheet, 1:4)
      createFreezePane(sheet, 1, 0)
    }
    sheet <- createSheet(wb, "imperfect match")
    if (nrow(imperfect_match) > 0) {
      addDataFrame(imperfect_match, sheet, row.names = FALSE)
      autoSizeColumn(sheet, 1:4)
      createFreezePane(sheet, 1, 0)
    }
    sheet <- createSheet(wb, "no match")
    if (nrow(no_match) > 0) {
      addDataFrame(no_match, sheet, row.names = FALSE)
      autoSizeColumn(sheet, 1:2)
      createFreezePane(sheet, 1, 0)
    }
    saveWorkbook(wb, match_file)
        
    return()
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
# INPUT
#   code            the new table code object
#   base            code for basis table, selection of rows with either 
#                   Select = TRUE or some text in code.
#   base_all_keys   all keys in the original base table
#   base_all_titles all titles in the origin base table
#' @importFrom stringdist amatch
#' @importFrom stringr str_match
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
  
  convert_title <- function(x) {
    x <- tolower(x)
    return(gsub("[  ,;.:]", "", x))
  }
  
  code_titles <- convert_title(code$Title)
  base_titles <- convert_title(base$Title)
  base_all_titles <- convert_title(base_all_titles)
  
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
    # Check if the titles are the same, ignoring leading numbers
    code_titles <- sub("^\\d+", "", code_titles[missing_code_rows])
    base_titles <- sub("^\\d+", "", base_titles[missing_base_rows])
    
    match_title <- amatch_unique(code_titles, base_titles, method = "jw", 
                                 maxDist = 0.2)
    title_code_rows <- which(!is.na(match_title))
    title_base_rows <- match_title[!is.na(match_title)]
    
    # calculate indices with respect to data frames code and base
    title_code_rows <- missing_code_rows[title_code_rows]
    title_base_rows <- missing_base_rows[title_base_rows]
    
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

# This function works as amatch, but only matches unique elements in x and y.
# TODO: is is possible to implement this more efficiently
amatch_unique <- function(x, y, ...) {
  
  # replace duplicated in y with NA  
  y_dupl <- y[duplicated(y)]
  y[y %in% y_dupl] <- NA
  
  result <- amatch(x, y, ...)
  
  result_dupl <- result[duplicated(result)]
  result[result %in% result_dupl] <- NA
  
  return(result)
}
