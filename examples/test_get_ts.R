library(cbsots)

id <- "83693NED"
id <- "82522NED"

ts_code_file <- "tscode/tscode.rds"
#edit_ts_code(ts_code_file)
ts_code <- readRDS(ts_code_file)

x <- get_ts(id, ts_code = ts_code, refresh = TRUE, download = FALSE, 
            include_meta = TRUE,
            frequencies = "q")


write_table_ts_xlsx(x, file.path("output", paste0("ts_", id, ".xlsx")), 
                     rowwise = TRUE)
