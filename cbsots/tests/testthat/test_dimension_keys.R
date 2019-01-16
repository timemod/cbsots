library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts: test for dimension keys")

options(encoding = "native.enc")

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "81974NED"

ts_code <- readRDS("tscode/tscode.rds")

raw_cbs_dir <- tempdir()
# result
result1 <- expect_output(get_ts(id, ts_code, refresh = TRUE,
                                raw_cbs_dir = raw_cbs_dir))

test_that("select a new key", {
  
  # select a new key programmatically
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$AfzetInvoerEnVerbruik[1, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$AfzetInvoerEnVerbruik[1, 3] <- "p040"

  # we expect output, because a new key has been added
  result2 <- expect_output(get_ts(id, ts_code_tmp, refresh = FALSE,
                                                 raw_cbs_dir = raw_cbs_dir))

  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})