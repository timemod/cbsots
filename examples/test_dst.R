library(cbsots)
library(testthat)

rm(list = ls())

id <- "80590ned"

ts_code <- readRDS("tscode/NLdata_dst.rds")
x <- get_ts(id, ts_code, frequencies = "M", download = TRUE, min_year = 2003)