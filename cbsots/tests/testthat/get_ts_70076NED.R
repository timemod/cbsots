library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 70076NED")

ts_code <- readRDS("tscode/tscode2.rds")

source("utils/check_ts_table.R")

id <- "70076ned"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
   
   result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE))
   
   check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
   expect_true(check$equal)
})