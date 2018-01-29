library(xlsx)

tables <- readRDS("tscodes.rds")

make_excel <- function(table_id, table) {
  cat("table_id\n")
  print(table_id)
  print(table)
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Topic")
  addDataFrame(table$Topic, sheet, row.names = FALSE)
  autoSizeColumn(sheet, 1:2)
  createFreezePane(sheet, 2, 1)
  if (length(table) > 1) {
    dimensies <- names(table)[-1]
    for (i in seq_along(dimensies)) {
      sheetnaam <- substr(dimensies[i], 1, 31)
      sheet <- createSheet(wb, sheetnaam)
      addDataFrame(table[[dimensies[[i]]]], sheet, row.names = FALSE)
      autoSizeColumn(sheet, 1:3)
      createFreezePane(sheet, 2, 1)
    }
  }
  saveWorkbook(wb, paste0(table_id, ".xlsx"))
  return(invisible(NULL))
}

for (name in names(tables)) {
  make_excel(name,  tables[[name]])
}