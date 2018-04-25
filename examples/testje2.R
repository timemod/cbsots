library(cbsots)

ts_code_file <- "tscode/tscode_testje2.rds"

ts_code <- readRDS(ts_code_file)

id <- "82595NED"
x <- get_ts(id, ts_code = ts_code, refresh = FALSE, include_meta = TRUE)
