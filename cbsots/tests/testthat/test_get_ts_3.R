library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts: no refresh and no existing raw data file")

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "7116shfo"

raw_cbs_dir <- tempdir()

test_that(id, {
  
  unlink(file.path(raw_cbs_dir, id), recursive = TRUE)
  
  result1 <- expect_output(get_ts(id, ts_code, refresh = FALSE,
                                  raw_cbs_dir = raw_cbs_dir, min_year = 2016))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})