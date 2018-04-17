library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82595NED example Andre")

ts_code <- readRDS("tscode/tscode_andre.rds")

source("utils/check_ts_table.R")

id <- "82595NED"

raw_cbs_dir <- tempdir()


test_that(id, {
  
  result1 <- expect_output(get_ts(id, ts_code, refresh = TRUE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})