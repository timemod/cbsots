#' @importFrom utils head
#' @export
print.cbs_table <- function(x, ...) {
  cat("cbs_table object\n")
  cat(sprintf("last modified : %s\n", as.character(x$last_modified)))
  cat(sprintf("order : %s\n", paste(x$order, collapse = ", ")))
  for (dim in names(x$codes)) {
    cat(sprintf("Dimension %s : \n", dim))
    print(head(x$codes[[dim]][, 1:3]))
  }
  cat("\n")
}