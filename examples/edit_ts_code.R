library(cbsots)

ts_code_file <- "tscode/tscode.rds"

ts_code <- readRDS(ts_code_file)
ts_code_small <- ts_code[1:3]
attr(ts_code_small, "class") <- "ts_code"
attr(ts_code_small, "package_version") <- "1.9.0"

ts_code_file <- "tscode/tscode_small.rds"
saveRDS(ts_code_small, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file, debug = TRUE)
