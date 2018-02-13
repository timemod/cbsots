#' Return the last time that the timeseries code for one or more tables has been
#' modified. 
#' @param id table id a character vector with CBS table identifiers
#' @param ts_code a \code{ts_code} object. This object can be created 
#' with a Shiny app that you start with function
#' \code{\link{edit_ts_code}}
#' @return  the last time that the code for the CBS tables have been modified.
#' @importFrom regts as.regts
#' @importFrom regts update_ts_labels
#' @importFrom stats as.formula
#' @importFrom cbsodataR get_data
#' @export
get_last_modified <- function(id, ts_code) {

  ts_code <- convert_ts_code(ts_code)
  
  if (!inherits(ts_code, "ts_code")) {
    stop("Argument ts_code is not a ts_code object")
  }
  
  if (length(id) == 1) {
    return(ts_code$table_code[[id]]$last_modified)
  } else {
    return(lapply(id, FUN = function(x) {ts_code$table_code[[x]]$last_modified}))
  }
}