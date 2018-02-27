library(cbsots)

source("functions/check_ts_table.R")

id <- "83693NED"
id <- "82522NED"
#d <- "00372"
#id <- "00372"

ts_code_file <- "tscode/tscode_testje.rds"
ts_code <- readRDS(ts_code_file)

print(get_last_modified(id, ts_code))
      
x <- get_ts(id, ts_code = ts_code, refresh = FALSE, include_meta = TRUE,
            start_year = 2015)

write_table_ts_xlsx(x, file.path("output", paste0("ts_", id, ".xlsx")))

ret <- check_ts_table(x, id)