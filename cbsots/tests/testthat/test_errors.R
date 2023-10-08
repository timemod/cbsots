library(cbsots)
library(testthat)

rm(list = ls())


ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "81974NED"

options(encoding = "native.enc")

ts_code <- readRDS("tscode/tscode.rds")

raw_cbs_dir_tmp <- tempfile(pattern = "raw_cbs_dir")
stopifnot(dir.create(raw_cbs_dir_tmp))

copy_raw_cbs_data <- function() {
  stopifnot(unlink(file.path(raw_cbs_dir_tmp, "*"), recursive = TRUE) == 0)
  stopifnot(file.copy(file.path("raw_cbs_data", id), raw_cbs_dir_tmp,
            recursive = TRUE))
  return(invisible())
}

copy_raw_cbs_data()

test_that("duplicate code for refresh = TRUE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 3] <- "x1"
  msg <- "Duplicate codes found for Topic:\nx1\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = raw_cbs_dir_tmp, msg)))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 3] <- "b__sl"
  msg <- "Duplicate codes found for ProductenPRODCOM:\nb__sl\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = raw_cbs_dir_tmp), msg))
})

test_that("unknown key for refresh = TRUE", {

  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 1] <- "xxx"
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 5] <- "xxx"

  msg <- "Unknown keys in code for dimension Topic in table 81974NED:\nxxx\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = raw_cbs_dir_tmp), msg))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 1] <- "aaa"
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 5] <- "aaa"
  msg <- "Unknown keys in code for dimension ProductenPRODCOM in table 81974NED:\naaa\n."
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE), msg))
})

test_that("no timeseries selected for refresh = TRUE", {
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic$Select <- FALSE
  msg <- "No single key selected for Topic"
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = raw_cbs_dir_tmp), msg))
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM$Select <- FALSE
  msg <- "No single key selected for ProductenPRODCOM"
  expect_output(expect_error(get_ts(id, ts_code_tmp, refresh = TRUE,
                                    raw_cbs_dir = raw_cbs_dir_tmp), msg))
})

test_that("duplicate code for refresh = FALSE", {
  
  copy_raw_cbs_data()
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$Topic[2, 3] <- "x1"
  emsg <- "Duplicate codes found for Topic:\nx1\n."
  expect_silent(
    expect_error(
      get_ts(id, ts_code_tmp, refresh = FALSE,
             raw_cbs_dir = raw_cbs_dir_tmp),
      emsg)
  )
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 2] <- TRUE
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[2, 3] <- "b__sl"
  emsg <- "Duplicate codes found for ProductenPRODCOM:\nb__sl\n."
  
  # now the data will be downloaded again, because ProductenPRODCOM[2, 2]
  # was not selected in the original download
  expect_output(
    expect_error(
      get_ts(id, ts_code_tmp, refresh = FALSE,
             raw_cbs_dir = raw_cbs_dir_tmp), 
      emsg),
   "Downloading table") 

  
  # next check: download == FALSE  
  emsg <- paste("^The files in directory .+ are incomplete or corrupt\\.",
                "Please download the data again\\.")

  expect_silent(
    expect_error(
      get_ts(id, ts_code_tmp, refresh = FALSE, download = FALSE,
             raw_cbs_dir = raw_cbs_dir_tmp), 
      emsg))
})

test_that("unknown key for refresh = FALSE", {
  
  copy_raw_cbs_data()
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 1] <- "xxx"
  ts_code_tmp$table_code[[id]]$codes$Topic[1, 5] <- "xxx"
  emsg <- "Unknown keys in code for dimension Topic in table 81974NED:\nxxx\n."
  expect_output(
    expect_error(get_ts(id, ts_code_tmp, refresh = FALSE,
                        raw_cbs_dir = raw_cbs_dir_tmp), 
                 emsg),
    "Downloading table") 
  
  # now check download = FALSE
  emsg <- paste("^The files in directory .+ are incomplete or corrupt\\.",
                "Please download the data again\\.")
  expect_silent(
    expect_error(
      get_ts(id, ts_code_tmp, refresh = FALSE, download = FALSE,
             raw_cbs_dir = raw_cbs_dir_tmp), 
      emsg))
    
    
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 1] <- "aaa"
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM[1, 5] <- "aaa"
  emsg <- "Unknown keys in code for dimension ProductenPRODCOM in table 81974NED:\naaa\n."
  expect_output(
    expect_error(get_ts(id, ts_code_tmp, refresh = FALSE,
                        raw_cbs_dir = raw_cbs_dir_tmp), emsg),
    "Downloading table") 
})

test_that("no timseries selected  for refresh = FALSE", {
  
  copy_raw_cbs_data()
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$Topic$Select <- FALSE
  msg <- "No single key selected for Topic"
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE,
                      raw_cbs_dir = raw_cbs_dir_tmp), msg)
  
  ts_code_tmp <- ts_code
  ts_code_tmp$table_code[[id]]$codes$ProductenPRODCOM$Select <- FALSE
  msg <- "No single key selected for ProductenPRODCOM"
  expect_error(get_ts(id, ts_code_tmp, refresh = FALSE,
                      raw_cbs_dir = raw_cbs_dir_tmp), msg)
})
