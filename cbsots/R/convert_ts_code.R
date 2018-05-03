convert_ts_code <- function(ts_code) {

  if (ts_code$package_version == "0.1") {
    # in version 0.2 the object class "table_code_collection" was renamed
    # to "ts_code"
    if (inherits(ts_code, "table_code_collection")) {
      class(ts_code) <- "ts_code"
    }
  }
  
  if (ts_code$package_version < "0.4") {
    # in versions prior to 0.4 each table code had an last_modified attribute.
    # this is not used any more

    ts_code$table_code[names(ts_code$table_code)] <- 
          lapply(ts_code$table_code, FUN = function(x) 
            {x$last_modified <- NULL
             return(x)})
  }
  
 
  return(ts_code)
}