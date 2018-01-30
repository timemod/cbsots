library(cbsots)

ts_code_file_rob <- "tscodes/tscodes_rob.rds"
ts_code_file_fred <- "tscodes/tscodes_fred.rds"

ts_code_file <- "tscodes/tscodes.rds"

file.copy(ts_code_file_fred, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file)