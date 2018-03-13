library(cbsots)

ts_code_file_basis <- "tscode_ernest.rds"
ts_code_file <- "tscode_ernest_tmp.rds"

tscodes <- readRDS(ts_code_file_basis)

saveRDS(tscodes, ts_code_file)

edit_ts_code(ts_code_file = ts_code_file, debug = FALSE)

