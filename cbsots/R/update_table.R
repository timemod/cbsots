# internal function: fill in Select and Code from an old table into table.
# Use common dimension names and Keys or Titles.
update_table <- function(table, old_table) {
  
  update_code <- function(dimension) {
    
    code <- table$codes[[dimension]]
    base_code <- old_table$codes[[dimension]]
    
    # Find matching keys and/or titles, and update code based on base_code
    # TODO: only update Select and Code if base_code has Select = TRUE and Code 
    # is a non-empty string.
    ret <- match_keys_and_titles(code, base_code)
    code[ret$code_rows, "Select"] <- base_code[ret$base_rows, "Select"]
    code[ret$code_rows, "Code"] <- base_code[ret$base_rows, "Code"]
    
    # Check if all previously selected variables have been found
    orig_selected <- which(base_code$Select)
    missing <- setdiff(orig_selected, ret$base_rows)
    if (length(missing) > 0) {
      warning(paste0("No matching entries found for dimension ", dimension,
                     ":\n",
                     paste(paste(base_code$Key[missing], 
                                 paste0("\"", base_code$Title[missing], "\""), 
                                 sep = " - "),
                           collapse = "\n")))
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
  
  return(table)
}

# Internal function: find matching keys and/or titles.
# Keys have to match exactly, titles approximately.
# The key takes precedence over the Title.
#' @importFrom stringdist amatch
match_keys_and_titles <- function(code, base) {
  
  # first check keys
  match_key <- match(code$Key, base$Key)
  code_rows <- which(!is.na(match_key))
  base_rows <- match_key[!is.na(match_key)]
  
  # now check matching titles for the rows that have no match yet
  missing_code_rows <- setdiff(seq_len(nrow(code)), code_rows)
  missing_base_rows <- setdiff(seq_len(nrow(base)), base_rows)
  
  convert_char <- function(x) {
    x <- tolower(x)
    return(gsub("[  ,;.:]", "", x))
  }
  
  if (length(missing_code_rows) > 0) {
    # Check if the titles are the same, ignoring spaces and some
    # punctuation characters.
    code_titles <- convert_char(code$Title[missing_code_rows])
    base_titles <- convert_char(base$Title[missing_base_rows])
    
    match_title <- amatch(code_titles, base_titles, method = "jw", 
                          maxDist = 0.2)
    title_code_rows <- which(!is.na(match_title))
    title_base_rows <- match_title[!is.na(match_title)]
    
    # title_code_rows and title_base_rows are the row numbers for the
    # row selection missing_code_rows and missing_base_rows. Now get the row 
    # number of original base and code data frames, and add to the original
    # code rows.
    code_rows <- c(code_rows, missing_code_rows[title_code_rows])
    base_rows <- c(base_rows, missing_base_rows[title_base_rows])
  }
  return(list(code_rows = code_rows, base_rows = base_rows))
}