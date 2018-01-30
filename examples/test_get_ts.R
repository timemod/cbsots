library(cbsots)

source("functions/check_ts_table.R")

id <- "83693NED"
id <- "82522NED"
id <- "00376"
ts_code_file <- "tscode/tscode_example1.rds"

table_code_collection <- readRDS(ts_code_file)

x <- get_ts(id, table_code_collection = table_code_collection, download = TRUE)
print(x)

write_table_ts_xlsx(x, file.path("output", paste0("ts_", id, ".xlsx")))


ret <- check_ts_table(x, id)