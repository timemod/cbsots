#' @importFrom utils head
#' @export
print.table_code <- function(x, ...) {
  cat("table_code object\n")
  cat(sprintf("last modified : %s\n", as.character(x$last_modified)))
  cat(sprintf("order : %s\n", paste(x$order, collapse = ", ")))
  for (dim in names(x$codes)) {
    cat(sprintf("Dimension %s : \n", dim))
    print(head(x$codes[[dim]][, 1:3]))
  }
  cat("\n")
}

#' @export
print.table_code_collection <- function(x, ...) {
  cat("table_code_collection object\n")
  cat(sprintf("package version = : %s\n", x$package_version))
  for (i in seq_along(x$table_code)) {
    cat(sprintf("Table id : %s\n", names(x$table_code))[i])
    tab <- x$table_code[[i]]
    print(tab)
  }
}