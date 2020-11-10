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
    
    
    prefix <- if (old_table_id == table_id) {
      table_id
    } else {
      paste0(old_table_id, "_",  table_id)
    }
    
    match_dir <- "match_reports"
    match_file <- file.path(match_dir, paste0(prefix, "_", dimension, ".xlsx"))
    
    # check missing elements  
    missing <- setdiff(seq_len(nrow(base)), matches$base_rows)
    
    
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
                           code = base$Code[missing],
                           stringsAsFactors = FALSE)
    
    #
    # warnings
    #
    
    advice <- paste0("Check ", match_file, ".") 
    
    if (nrow(base) > 0 && length(matches$base_rows) == 0) {
      warning(paste0("No matching entries found for dimension ", dimension, 
                     ".\n", advice))
    } else if (length(missing) > 0) {
      warning(paste0("No matching entries found for dimension ", dimension,
                     ":\n",
                     paste(paste(base$Key[missing], 
                                 paste0("\"", base$Title[missing], "\""), 
                                 sep = " - "),
                           collapse = "\n"),
                     ".\n", advice))
      
    } else if (nrow(imperfect_match) > 0) {
      warning(paste0("Imperfect matches found for dimension ", dimension, 
                     ".\n", advice))
    }
    
    #
    # write a match report
    #
    
    if (!dir.exists(match_dir)) {
      dir.create(match_dir)
    }
    
    wb <- createWorkbook()
    
    sheet <- "perfect match"
    addWorksheet(wb, sheet)
    if (nrow(perfect_match) > 0) {
      writeData(wb, sheet, perfect_match, rowNames = FALSE)
      setColWidths(wb, sheet, 1:4, widths = "auto")
      freezePane(wb, sheet, 2, 1)
    }
    
    sheet <- "imperfect match"
    addWorksheet(wb, sheet)
    if (nrow(imperfect_match) > 0) {
      writeData(wb, sheet, imperfect_match, rowNames = FALSE)
      setColWidths(wb, sheet, 1:4, widths = "auto")
      freezePane(wb, sheet, 2, 1)
    }
    
    sheet <- "no match"
    addWorksheet(wb, sheet)
    if (nrow(no_match) > 0) {
      writeData(wb, sheet, no_match, rowNames = FALSE)
      setColWidths(wb, sheet, 1:3, widths = "auto")
      freezePane(wb, sheet, 2, 1)
    }
    
    minWidth_old <- options("openxlsx.minWidth")[[1]]
    options("openxlsx.minWidth" = 8.43)
    tryCatch({
      result <- saveWorkbook(wb, match_file, overwrite = TRUE, returnValue = TRUE)
      if (!isTRUE(result)) stop(result$message, call. = FALSE)
    }, finally = {
      options("openxlsx.minWidth" = minWidth_old)
    })
    
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