#' Fill tables with values from another table.
#' 
#' @param tscodes a \code{ts_code} object. This object can be created
#' and modified with function \code{\link{edit_ts_code}}, which starts a Shiny
#' app
#' @param ids ids of tables to be filled from the second tabel
#' @param basis_id id of the table whose values will be used to fill
#' the first tables.
#' @export
fill_tables_from_table <- function(tscodes, ids,  basis_id) {
  
  basis_table <- tscodes$table_code[[basis_id]]$codes
  
  new_table_created <- FALSE
  
  for (id in  ids) {
    
    if (id %in% names(tscodes$table_code)) {
      table <- tscodes$table_code[[id]]$codes
    } else {
      table <- create_new_table(id)
      new_table_created <- TRUE
    }
    
    table <- fill_table_from_table(table, basis_table)
    
    tscodes$table_code[[id]]$codes <- table
  }
  
  if (new_table_created) {
    tscodes$table_code <- tscodes$table_code[order(names(tscodes$table_code))]
  }
  
  return(tscodes)
}

# internal function
fill_table_from_table <- function(table, basis_table) {
  
  fill_code_from_code <- function(name) {
    code <- table[[name]]
    basis_code <- basis_table[[name]]
    
    common_keys <- intersect(code$Key, basis_code$Key)
    rows <- match(common_keys, code$Key)
    basis_rows <- match(common_keys, basis_code$Key)
    code[rows, "Select"] <- basis_code[basis_rows, "Select"]
    code[rows, "Code"] <- basis_code[basis_rows, "Code"]
    code[rows, "Title"] <- basis_code[basis_rows, "Title"]
    table[[name]] <- code
    return(invisible(NULL))
  }
  common_names <- intersect(names(table), names(basis_table))
  dum <- lapply(common_names, FUN = fill_code_from_code)
  return(table)
}