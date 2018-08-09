convert_ts_code <- function(ts_code) {

  if (ts_code$package_version == "0.1") {
    # in version 0.2 the object class "table_code_collection" was renamed
    # to "ts_code"
    if (inherits(ts_code, "table_code_collection")) {
      class(ts_code) <- "ts_code"
    }
  }
  
  if (ts_code$package_version < "0.4") {
    
    # In versions prior to 0.4 each table code had a last_modified attribute.
    # This is not used any more, so remove it.
    
    remove_last_modified <- function(x) {
      x$last_modified <- NULL
      return(x)
    }

    ts_code$table_code <-  lapply(ts_code$table_code, 
                                  FUN = remove_last_modified) 
  }
  
  if (ts_code$package_version < "0.5") {
    
    # Versions prior to version 0.5 did not have a cbs_key_order attribute
    # Therefore create it based on the comparison of the Key and OrigKeyOrder
    # columns.
    
    has_cbs_key_order <- function(dimension_code) {
      return(identical(dimension_code$Key, dimension_code$OrigKeyOrder))
    }
    
    create_cbs_key_order  <- function(table_code) {
      
      new_table_code <- table_code
      
      new_table_code$cbs_key_order <- sapply(new_table_code$codes, 
                                             FUN = has_cbs_key_order)
      
      # reorder the elements of the list
      new_table_code <- new_table_code[c("short_title", "order", 
                                         "cbs_key_order", "codes")]
      
      class(new_table_code) <- class(table_code)
    
      return(new_table_code)
    }
    
    
    ts_code$table_code <- lapply(ts_code$table_code, 
                                  FUN = create_cbs_key_order) 
  }
  
  
  ts_code$package_version <- packageVersion("cbsots")
  
  return(ts_code)
}