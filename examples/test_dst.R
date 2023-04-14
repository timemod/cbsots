library(cbsots)
library(testthat)

rm(list = ls())

id <- "80590ned"

ts_code <- readRDS("tscode/NLdata_dst.rds")

# door gebruik te maken van dez e code kunnen we checken
x <- get_ts(id, ts_code, frequencies = "M", download = TRUE, min_year = 2003)

x <- get_ts(id, ts_code, frequencies = "M", download = TRUE, min_year = 2022)

#x <- get_ts(id, ts_code, frequencies = "M", download = TRUE, min_year = 2022)
