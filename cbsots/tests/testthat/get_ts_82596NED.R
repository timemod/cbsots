library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82596NED")

ts_code_file <- "tscode/tscode_82596NED.rds"

#  edit_ts_code(ts_code_file)

ts_code <- readRDS(ts_code_file)

source("utils/check_ts_table.R")

id <- "82596NED"

raw_cbs_dir <- "raw_cbs_dir"

test_that(id, {
  
  result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, 
                          min_year = 2015, raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})