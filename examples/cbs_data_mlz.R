rm(list = ls())

library(cbsots)

ts_code_file <- "tscode/tscode_mlz.rds"
ts_code <- readRDS(ts_code_file)

table_ids <- names(ts_code)

for (table_id in table_ids) {
  
  data <- get_ts(table_id, ts_code = ts_code, download = TRUE, 
                 base_url = "https://dataderden.cbs.nl")
  
  # store the timeseries in an Excel file
  write_table_ts_xlsx(data, 
                      file.path("output/mlz", paste0("ts_", table_id, ".xlsx")), 
                      rowwise = TRUE)
}
