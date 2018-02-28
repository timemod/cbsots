library(cbsots)
library(testthat)

rm(list = ls())

# 
# In this test we check the refresh and min_year arguments. Note that we cannot 
# compare the results with expected results, because the table data is downloaded
# from the CBS and the table may change over time.
#
context("get_ts: refresh and min_year")

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "83460NED"

raw_cbs_dir <- tempdir()

test_that(id, {
  
  result1 <- expect_message(expect_output(get_ts(id, ts_code, refresh = TRUE,
                                                 raw_cbs_dir = raw_cbs_dir)))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  
  result2 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, 
                                  raw_cbs_dir = raw_cbs_dir))
  expect_identical(result1, result2)
  
  result3 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, min_year = 2010,
                                  raw_cbs_dir = raw_cbs_dir))
  expected_result_2010 <- result1
  expected_result_2010$Q <- expected_result_2010$Q["2010/"]
  expected_result_2010$Y <- expected_result_2010$Y["2010/"]
  expect_identical(result3, expected_result_2010)
  
  result4 <- expect_message(expect_output(get_ts(id, ts_code, refresh = TRUE,
                                                 min_year = 2010,
                                                 raw_cbs_dir = raw_cbs_dir)))
  expect_identical(result4, expected_result_2010)
  
  # Now we want the timeseries again for all years. We expect some output, 
  # because the data has to be downloaded again:
  # the last download only downloaded data starting from 2010
  result5 <- expect_message(expect_output(get_ts(id, ts_code, refresh = FALSE,
                                                 raw_cbs_dir = raw_cbs_dir)))
  expect_identical(result5, result1)
})