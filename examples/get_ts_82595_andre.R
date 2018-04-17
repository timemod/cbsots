library(cbsots)

source("functions/check_ts_table.R")

id <- "82595NED"

ts_code_file <- "tscode/tscode_andre.rds"
ts_code <- readRDS(ts_code_file)

x <- get_ts(id, ts_code = ts_code, refresh = TRUE, include_meta = TRUE,
            min_year = 2008)
#x$Q <- NULL
#x$M <- NULL

check <- check_ts_table(x, id, raw_cbs_dir = "raw_cbs_data")


write_table_ts_xlsx(x, file = "jan.xlsx")