#' @export
print.ts_code <- function(x, ...) {
  cat("ts_code object\n")
  cat(sprintf("package version = : %s\n", x$package_version))
  for (i in seq_along(x$table_code)) {
    cat(sprintf("Table id : %s\n", names(x$table_code))[i])
    tab <- x$table_code[[i]]
    print(tab)
  }
}

#' @importFrom utils head
#' @export
print.table_code <- function(x, ...) {
  cat("ts_code_table object\n")
  cat(sprintf("Order : %s\n", paste(x$order, collapse = ", ")))
  cat("The first 6 rows and 3 columns are shown.\n")
  for (dim in names(x$codes)) {
    cat(sprintf("Dimension %s : \n", dim))
    print(head(x$codes[[dim]][, 1:3]))
  }
  cat("\n")
}

#' @importFrom regts topleft
#' @export
print.table_ts <- function(x, ...) {
  cat("table_ts object\n")
  freqs <- setdiff(names(x), c("ts_names", "meta"))
  for (freq in freqs) {
    cat(sprintf("Frequency %s :\n", freq))
    cat("Topleft part of the result (the first 6 rows and 10 columns):\n")
    print(topleft(x[[freq]], ncol = min(10, ncol(x[[freq]]))))
  }
}