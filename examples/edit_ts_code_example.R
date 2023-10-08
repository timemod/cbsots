rm(list = ls())
library(cbsots)

tmpfile <- tempfile()
saveRDS(ts_code_example, tmpfile)
edit_ts_code(tmpfile)
