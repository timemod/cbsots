get_table_id <- function(table_desc, table_ids) {

  table_id <- table_ids[table_desc]
 
  # error message
  if (is.na(table_id)) {
    showModal(modalDialog(
      title = "Internal error",
      paste0("Table \"", table_desc, "\" not found in list of tables"),
      easyClose = TRUE
    )) 
    cat("Table \"", table_desc, "\" not found in list of tables")
    cat("Current list of tables\n")
    print(table_ids)
  }
  
  return(table_id)
}