#' Fill tables with values from another table.
#' 
#' This function fills in the Select and Code fields from a base table based
#' on common Keys or Titles. The function tries to find matching keys or titles
#' and updates the corresponding Select en Code fields. 
#' The 
#' function creates a match report in directory \code{match_reoport} 
#' for each table and dimension. The match report is an Excel file with a name
#' composed of the table id of the base table and the new
#' table, and the and the dimension, e.g. 
#' \code{83186NED_83361NED_TypeZelfstandige.xlsx}.
#' This function is still experimental and you should always check the 
#' results carefully.
#' 
#' @param ts_code a \code{ts_code} object. This object can be created
#' and modified with function \code{\link{edit_ts_code}}, which starts a Shiny
#' app
#' @param ids ids of tables to be filled from the base tabel
#' @param base_id the id of the base table
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @export
fill_tables_from_table <- function(ts_code, ids,  base_id, base_url = NULL) {
  
  ts_code <- convert_ts_code(ts_code)
 
  base_table <- ts_code[[base_id]]
  
  new_table_created <- FALSE
  
  for (id in  ids) {
    
    if (id %in% names(ts_code)) {
      table <- ts_code[[id]]
    } else {
      table <- create_new_table(id, base_url)
      new_table_created <- TRUE
    }
    
    table <- update_table(table, base_table, id, base_id)
  
    ts_code[[id]] <- table
    
  }
  
  if (new_table_created) {
    # order tables alphabetically
    ts_code[] <- ts_code[order(names(ts_code))]
  }
  
  return(ts_code)
}