#' Fill tables with values from another table.
#' 
#' This function fills in the Select and Code fields from a base table based
#' on common Keys or Titles. The function tries to find matching keys or titles
#' and updates the corresponding Select en Code fields. This function is still
#' experimental and you should always check the results carefully.
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
  
  tscodes <- convert_ts_code(tscodes)
 
  base_table <- tscodes$table_code[[base_id]]
  
  new_table_created <- FALSE
  
  for (id in  ids) {
    
    if (id %in% names(tscodes$table_code)) {
      table <- tscodes$table_code[[id]]
    } else {
      table <- create_new_table(id, base_url)
      new_table_created <- TRUE
    }
    
    table <- update_table(table, base_table, id, base_id)
  
    tscodes$table_code[[id]] <- table
    
  }
  
  if (new_table_created) {
    tscodes$table_code <- tscodes$table_code[order(names(tscodes$table_code))]
  }
  
  return(tscodes)
}
