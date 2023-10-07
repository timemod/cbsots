#' @export
print.ts_code <- function(x, ...) {
  cat("ts_code object\n")
  cat(sprintf("package version = : %s\n", attr(x, "package_version")))
  table_ids <- names(x)
  for (i in seq_along(x)) {
    cat(sprintf("Table id : %s\n", table_ids[i]))
    print(x[[i]])
  }
}

#' @importFrom utils head
#' @export
print.table_code <- function(x, ...) {
  cat("ts_code_table object\n")
  cat(sprintf("id          : %s\n", x$id))
  cat(sprintf("Short title : %s\n", x$short_title))
  cat(sprintf("Order       : %s\n", paste(x$order, collapse = ", ")))
  cat("cbs_key_order for each dimension:\n")
  print(x$cbs_key_order)
  cat("\n")
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