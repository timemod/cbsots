#' Update tables
#' 
#' This function updates one or more tables in a \code{ts_code} object.
#' The function downloads the latest meta data from the CBS website
#' and tries to find  matching keys or titles in the old and new table. The 
#' function creates a match report in directory \code{match_reoport} 
#' for each table and dimension. The match report is an Excel file with a name
#' composed of the table id and dimension, e.g. \code{81810NED_Topic.xlsx}.
#' This function is still experimental and you should always check the results 
#' carefully.
#' 
#' @param ts_code a \code{ts_code} object. This object can be created
#' and modified with function \code{\link{edit_ts_code}}, which starts a Shiny
#' app
#' @param ids ids of tables to be updated
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @export
update_tables <- function(ts_code, ids, base_url = NULL) {
 
  ts_code <- convert_ts_code(ts_code)
  
  for (id in  ids) {
    
    if (id %in% names(ts_code)) {
      table <- ts_code[[id]]
    } else {
      stop(paste("Table", id, "not in list of tables"))
    }
    
    new_table <- create_new_table(id, base_url)
    
    table <- update_table(new_table, table, id, id)
    
    ts_code[[id]] <- table
    
  }
  
  return(ts_code)
}
