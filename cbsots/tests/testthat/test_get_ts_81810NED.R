library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 81810NED")


ts_code <- readRDS("tscode/tscode2.rds")

#edit_ts_code("tscode/tscode2.rds")

source("utils/check_ts_table.R")
source("utils/read_match_report.R")

id <- "81810NED"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  ts_code_new <- update_tables(ts_code, ids = id)
 
  result1 <- expect_silent(get_ts(id, ts_code_new, refresh = FALSE))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  
  expect_known_value(read_match_report(ts_code, id),
                     file = file.path("expected_output", 
                                      paste0(id, "_match_report.rds")))
})

