#  Converts a ts_code object created with older versions to cbsots to 
#  a the ts_code object as used in the current version of cbsots. 
#  Returns NULL if ts_code if not a valid ts_code object.
convert_ts_code <- function(ts_code) {
  
  if (!inherits(ts_code, "ts_code") &&
      !inherits(ts_code, "table_code_collection")) {
    # In version 0.2 the object class "table_code_collection" was renamed
    # to "ts_code", so we should still check for classes of the type 
    # table_code_collection.
    return(NULL)
  }

  if (is.null(attr(ts_code, "package_version"))) {
    
    # Since version 1.2 a ts_code objects is a list with an attribute 
    # package_version. In earlier versions ts_code was a list of length
    # 2, with element package_version and element table_code.
    
    old_package_version <- ts_code$package_version
  
    ts_code <- create_ts_code(ts_code$table_code)
    
    if (old_package_version < "0.4") {
      
      # In versions prior to 0.4 each table had a last_modified attribute.
      # This is not used any more, so remove it.
      
      remove_last_modified <- function(x) {
        x$last_modified <- NULL
        return(x)
      }
      ts_code[] <- lapply(ts_code, FUN = remove_last_modified) 
    }
    
    if (old_package_version < "0.5") {
      
      # Versions prior to version 0.5 did not have a cbs_key_order attribute
      # Therefore create it based on the comparison of the Key and OrigKeyOrder
      # columns.
      
      has_cbs_key_order <- function(dimension_code) {
        return(identical(dimension_code$Key, dimension_code$OrigKeyOrder))
      }
      
      create_cbs_key_order  <- function(table) {
        
        new_table <- table
        
        new_table$cbs_key_order <- sapply(new_table$codes, 
                                          FUN = has_cbs_key_order)
        
        # reorder the elements of the list
        new_table <- new_table[c("short_title", "order", "cbs_key_order", 
                                 "codes")]
        
        class(new_table) <- class(table)
        
        return(new_table)
      }
      
      
      ts_code[] <- lapply(ts_code, FUN = create_cbs_key_order) 
    }
    
  } else {

    # ts_code has now been updated to new package version, so modify
    # the package_version attribute
   
    attr(ts_code, "package_version") <- packageVersion("cbsots")
  
  }
  
  reorder_dimension <- function(dimension, table_code) {
    cbs_order <- table_code$cbs_key_order[[dimension]]
    return(order_code_rows(table_code$codes[[dimension]], 
                           cbs_order = cbs_order))
  }
  
  reorder_table_code <- function(table_code) {
    codes <- table_code$codes
    dimensions <- names(codes)
    table_code$codes[] <- lapply(dimensions, FUN = reorder_dimension, 
                                table_code = table_code)
    return(table_code)
  } 

  ts_code[] <- lapply(ts_code, FUN = reorder_table_code) 

  return(ts_code)
}