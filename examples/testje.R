library(cbsots)

ts_code_file_basis <- "tscode/tscode_fred.rds"
ts_code_file <- "tscode/tscode_testje.rds"


tscodes <- readRDS(ts_code_file_basis)


saveRDS(tscodes, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file, debug = FALSE)
