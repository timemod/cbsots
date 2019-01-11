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
  
  result1 <- expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir))
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
  
  result1 <- expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                  min_year = 2017, frequencies = "Y",
                                  raw_cbs_dir = raw_cbs_dir, 
                                  download_all_keys = TRUE))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result1$Y), 2)
  
  result2 <- expect_silent(get_ts(id, ts_code_3, refresh = FALSE, 
                                  min_year = 2017, frequencies = "Y", 
                                  raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
  expect_equal(ncol(result2$Y), 6)
})

test_that(paste(id, "deleted keys"), {

  # Remove key Totaal_135. This key is not selected, but if such a key
  # has been removed the other Totaal__ variables may describe a different
  # series, so we expect an error.
  
  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- topic[Key != 'InvoerVanGoederen_3']
  
  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic
  
  #
  # TODO: check_ts_table gives an error if min_year = 2013 is used
  # below. Sometging must be wrong. What is going on?
  #
  msg <- paste("Keys in code for dimension Topic in table 82595NED do not",
               "agree with CBS keys.\nUpdate the table coding with the shiny",
               "application edit_ts_code.")
  expect_warning(
    result1 <- get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"),
    msg)
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})

test_that("non unique key differences", {

  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- topic[Key != 'Totaal_135']

  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  # this statement should give an error because of the running numbers
  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with CBS keys.\n",
                "The problem keys without running number are not unique.\n",
                "Update the table coding with the shiny",
                " application edit_ts_code.")
  expect_error(get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir),
               msg)
})

test_that("modified title", {
  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic$Title[151] <- "xxx"
   
  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic
  
  msg <- paste("Titles in code for dimension Topic in table 82595NED do not",
               "agree with CBS titles.\nUpdate the table coding with the shiny",
               "application edit_ts_code.")
  expect_warning(
    result1 <- get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"),
    msg)
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})
