# Internal function to create a ts_code object from a list of table_code obects.
create_ts_code <- function(tables = list()) {
  return(structure(tables, class = "ts_code", 
                   package_version = packageVersion("cbsots")))
}