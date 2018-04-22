library(cbsots)

source("functions/check_ts_table.R")

id <- "40027NED"

ts_code_file <- "tscode/tscode_mlz.rds"
ts_code <- readRDS(ts_code_file)

x <- get_ts(id, ts_code = ts_code, refresh = FALSE, include_meta = TRUE,
            base_url = "https://dataderden.cbs.nl")


write_table_ts_xlsx(x, file.path("output", paste0("ts_", id, ".xlsx")), 
                    rowwise = TRUE)

ret <- check_ts_table(x, id)
print(ret$equal)
