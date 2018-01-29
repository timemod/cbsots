library(cbsots)

id <- "83693NED"
id <- "82522NED"
ts_code_file <- "tscodes/tscodes_example1.rds"

table_code_collection <- readRDS(ts_code_file)

x <- get_ts(id, table_code_collection = table_code_collection, download = TRUE)