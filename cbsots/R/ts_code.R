# Internal function to create a ts_code object from a list of table_code obects.
new_ts_code <- function(tables = list()) {
  return(structure(tables, class = "ts_code", 
                   package_version = packageVersion("cbsots")))
}

#' Select tables from a `ts_code` object
#' @param tsc A `ts_code` object.
#' @param ids A character vector with table identifiers or a numeric vector
#' with the indices of the tables.
#' @return A new `ts_code` object with a selection of tables.
#' @examples
#' select_tables(ts_code_example, ids =  c("70076ned", "7116shfo"))
#' 
#' # this expression gives the same result:
#' select_tables(ts_code_example, ids = c(2,3))
#' @seealso `join_tables`
#' @export
select_tables <- function(tsc, ids) {
  return(new_ts_code(tsc[sort(ids)]))
}

#' Join two  `ts_code` objects.
#' 
#' The two objects should not have common table ids. Use function
#' \code{\link{select_tables}} to select the tables that should be joined.
#' @param tsc1 The first  `ts_code` object.
#' @param tsc2 The second  `ts_code` object.
#' @return A new `ts_code` object with the union of the tables in `ts_code`
#' objects `tsc1` and `tscs2`.
#' @examples
#' ts_code_1 <- select_tables(ts_code_example, ids =  c("70076ned", "7116shfo"))
#' ts_code_2 <- select_tables(ts_code_example, ids =  "00372")
#' join_tables(ts_code_1, ts_code_2)
#' @seealso `select_tables`
#' @export
join_tables <- function(tsc1, tsc2) {
  ids1 <- names(tsc1)
  ids2 <- names(tsc2)
  common_names <- intersect(ids1, ids2)
  if (length(common_names) > 0) {
    stop("The two ts_code object have common table ids",
         paste(paste0("\"", common_names, "\""), collapse = ", "), ".")
  }
  table_list <- c(tsc1, tsc2)
  table_list <- table_list[order(names(table_list))]
  return(new_ts_code(table_list))
}