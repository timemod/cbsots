library(cbsots)

ts_code_file <- "tscode_ernest.rds"
ts_codes <- readRDS(ts_code_file)


ts_codes$table_code[["83357NED"]] <- NULL

ret <- fill_tables_from_table(ts_codes,
                                ids = c("83357NED", "83361NED"),
                                basis_id = "83186NED")

saveRDS(ts_codes, "tmp.rds")

edit_ts_code(ts_code_file = "tmp.rds", debug = FALSE)

unlink("tmp.rds")

