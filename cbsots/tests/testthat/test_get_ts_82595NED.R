library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82595NED")

ts_code_file_1 <- "tscode/tscode_82595NED_1.rds"
ts_code_file_2 <- "tscode/tscode_82595NED_2.rds"
ts_code_file_3 <- "tscode/tscode_82595NED_3.rds"


ts_code_1 <- readRDS(ts_code_file_1)
ts_code_2 <- readRDS(ts_code_file_2)
ts_code_3 <- readRDS(ts_code_file_3)

source("utils/check_ts_table.R")

id <- "82595NED"

raw_cbs_dir <- tempdir()

test_that(id, {
  
  result1 <- expect_message(expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir)))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result1$Y), 2)
  
  result2 <- expect_silent(get_ts(id, ts_code_2, refresh = FALSE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result2$Y), 4)
})


test_that(paste(id, "download_all_keys"), {
  
  result1 <- expect_message(expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                                 min_year = 2017, 
                                                 raw_cbs_dir = raw_cbs_dir,
                                                 download_all_keys = TRUE)))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result1$Y), 2)
  
  result2 <- expect_silent(get_ts(id, ts_code_3, refresh = FALSE, 
                                  min_year = 2017, raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result2$Y), 6)
})
