#' Fill tables with values from another table.
#' 
#' This function fills in the Select and Code fields from a base table based
#' on common Keys or Titles.
#' 
#' @param tscodes a \code{ts_code} object. This object can be created
#' and modified with function \code{\link{edit_ts_code}}, which starts a Shiny
#' app
#' @param ids ids of tables to be filled from the base tabel
#' @param base_id the id of the base table
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @export
fill_tables_from_table <- function(tscodes, ids,  base_id, base_url = NULL) {
  
  base_table <- tscodes$table_code[[base_id]]
  
  new_table_created <- FALSE
  
  for (id in  ids) {
    
    if (id %in% names(tscodes$table_code)) {
      table <- tscodes$table_code[[id]]
    } else {
      table <- create_new_table(id, base_url)
      new_table_created <- TRUE
    }
    
    table <- fill_table_from_table(table, base_table)
  
    tscodes$table_code[[id]] <- table
    
  }
  
  if (new_table_created) {
    tscodes$table_code <- tscodes$table_code[order(names(tscodes$table_code))]
  }
  
  return(tscodes)
}

# internal function: fill in Select and Code based on common Keys or Titles
fill_table_from_table <- function(table, base_table) {
  
  fill_code_from_code <- function(dimension) {
    
    code <- table$codes[[dimension]]
    base_code <- base_table$codes[[dimension]]
    
    common_keys <- intersect(code$Key, base_code$Key)
    rows <- match(common_keys, code$Key)
    base_rows <- match(common_keys, base_code$Key)
    common_titles <- intersect(code$Title, base_code$Title)
    rows <- union(rows, match(common_titles, code$Title))
    base_rows <- union(base_rows, match(common_titles, base_code$Title))
    
    code[rows, "Select"] <- base_code[base_rows, "Select"]
    code[rows, "Code"] <- base_code[base_rows, "Code"]
    
    # If the keys were ordered according to original cbs ordering in the base
    # table, then they were not ordered in the origina table.
    # then the keys were ordered according to selected first.
    cbs_order <- identical(base_code$Key, base_code$OrigKeyorder)
    if (!cbs_order) {
      code <- order_code_rows(code, cbs_order)
    }

    return(code)
  }
  
  common_dimensions <- intersect(names(table$codes), names(base_table$codes))
  new_codes <- sapply(common_dimensions, FUN = fill_code_from_code, 
                      simplify = FALSE)
  table$codes <- modifyList(table$codes, new_codes)
  
  # also adapt the ordering
  ord <- match(base_table$order, table$order)
  ord <- ord[!is.na(ord)]
  ord <- c(ord, setdiff(seq_along(table$order), ord))
  table$order <- table$order[ord]
  
  return(table)
}