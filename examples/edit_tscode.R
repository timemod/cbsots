library(cbsots)

ts_code_file_rob <- "tscode/tscode_rob.rds"
ts_code_file_fred <- "tscode/tscode_fred.rds"

ts_code_file <- "tscode/tscode.rds"

file.copy(ts_code_file_fred, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file, debug = FALSE)