# internal function: fill in Select and Code from an old table into table.
# Use common dimension names and Keys or Titles.
update_table <- function(table, old_table) {
  
  update_code <- function(dimension) {
    
    code <- table$codes[[dimension]]
    base_code <- old_table$codes[[dimension]]
    
    common_keys <- intersect(code$Key, base_code$Key)
    rows <- match(common_keys, code$Key)
    base_rows <- match(common_keys, base_code$Key)
    
    skip_spaces <- function(x) {
      return(gsub(" ", "", x))
    }
   
    # Check if the titles are the same, but ignore spaces.
    # TODO: use package stringDist to find out which titles are 
    # closest. This seems to be rather complicated.
    code_titles_no_spaces <- skip_spaces(code$Title)
    base_code_titles_no_spaces <- skip_spaces(base_code$Title)

    
    common_titles <- intersect(code_titles_no_spaces, 
                               base_code_titles_no_spaces)
    rows <- union(rows, match(common_titles, code_titles_no_spaces))
    base_rows <- union(base_rows, match(common_titles, 
                                        base_code_titles_no_spaces))
    
    code[rows, "Select"] <- base_code[base_rows, "Select"]
    code[rows, "Code"] <- base_code[base_rows, "Code"]
    
    
    # check if all previously selected variables have been found
    orig_selected <- which(base_code$Select)
    missing <- setdiff(orig_selected, base_rows)
    if (length(missing) > 0) {
      warning(paste0("No matching entries found for dimension ", dimension,
                     ":\n",
                     paste(paste(base_code$Key[missing], paste0("\"", 
                                                                base_code$Title[missing], "\""), 
                                 sep = " - "),
                           collapse = "\n")))
    }
    
    # If the keys were ordered according to original cbs ordering in the base
    # table, then they were not ordered in the original table.
    # then the keys were ordered according to selected first.
    cbs_order <- identical(base_code$Key, base_code$OrigKeyorder)
    if (!cbs_order) {
      code <- order_code_rows(code, cbs_order)
    }
    
    return(code)
  }
  
  common_dimensions <- intersect(names(table$codes), names(old_table$codes))
  new_codes <- sapply(common_dimensions, FUN = update_code, simplify = FALSE)
  table$codes <- modifyList(table$codes, new_codes)
  
  # also adapt the ordering
  ord <- match(old_table$order, table$order)
  ord <- ord[!is.na(ord)]
  ord <- c(ord, setdiff(seq_along(table$order), ord))
  table$order <- table$order[ord]
  
  return(table)
}