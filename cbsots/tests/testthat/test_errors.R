library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts: test_errors")

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "81974NED"

ts_code <- readRDS("tscode/tscode.rds")

test_that("duplicate code for refresh = TRUE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 3] <- "x1"
  msg <- "Duplicate codes found for Topic:\nx1\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = tempdir()), msg))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 3] <- "b__sl"
  msg <- "Duplicate codes found for ProductenPRODCOM:\nb__sl\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = tempdir()), msg))
})

test_that("unknown key for refresh = TRUE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 1] <- "xxx"
  msg <- "Unknown keys in code for dimension Topic in table 81974NED:\nxxx\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = tempdir()), msg))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 1] <- "aaa"
  msg <- "Unknown keys in code for dimension ProductenPRODCOM in table 81974NED:\naaa\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE), msg))
})

test_that("no timseries selected  for refresh = TRUE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic$Select <- FALSE
  msg <- "No single key selected for Topic"
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = tempdir()), msg))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM$Select <- FALSE
  msg <- "No single key selected for ProductenPRODCOM"
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = tempdir()), msg))
})

test_that("duplicate code for refresh = FALSE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 3] <- "x1"
  msg <- "Duplicate codes found for Topic:\nx1\n."
  expect_error(
    expect_silent(
      get_ts(id, ts_code_tmp, refresh = FALSE)
    ), 
    msg)
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 3] <- "b__sl"
  msg <- "Duplicate codes found for ProductenPRODCOM:\nb__sl\n."
  expect_error(
    expect_output(
      get_ts(id, ts_code_tmp, refresh = FALSE), msg)
    )
})

test_that("unknown key for refresh = FALSE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 1] <- "xxx"
  msg <- "Unknown keys in code for dimension Topic in table 81974NED:\nxxx\n."
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE), msg)
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 1] <- "aaa"
  msg <- "Unknown keys in code for dimension ProductenPRODCOM in table 81974NED:\naaa\n."
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE), msg)
})

test_that("no timseries selected  for refresh = FALSE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic$Select <- FALSE
  msg <- "No single key selected for Topic"
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE), msg)
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM$Select <- FALSE
  msg <- "No single key selected for ProductenPRODCOM"
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE), msg)
})
