library(cbsots)

ts_code_file_basis <- "tscode/tscode_fred.rds"
ts_code_file <- "tscode/tscode_testje.rds"

tscodes <- readRDS(ts_code_file_basis)

tscodes$table_code <- tscodes$table_code[1:3]

saveRDS(tscodes, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file, debug = FALSE)
