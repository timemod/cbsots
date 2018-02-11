convert_ts_code <- function(ts_code) {

  if (ts_code$package_version == "0.1") {
    # in version 0.2 the object class "table_code_collection" was renamed
    # to "ts_code"
    if (inherits(ts_code, "table_code_collection")) {
      class(ts_code) <- "ts_code"
    }
  }
  
 
  return(ts_code)
}