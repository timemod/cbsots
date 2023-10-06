library(cbsots)

ts_code_file <- "tscode/tscode.rds"
ts_code <- readRDS(ts_code_file)

ts_code_small <- select_tables(ts_code, 1:3)
ts_code_file_small <- "tscode/tscode_small.rds"
saveRDS(ts_code_small, ts_code_file_small)

edit_ts_code(ts_code_file_small)
