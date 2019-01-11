library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82596NED")

ts_code_file <- "tscode/tscode_82596NED.rds"

#  edit_ts_code(ts_code_file)

ts_code <- readRDS(ts_code_file)

source("utils/check_ts_table.R")

id <- "82596NED"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  # TODO: controleer of de titles inderdaad niet meer overeenstemmen.
  msg <- paste("Titles in code for dimension Topic in table 82596NED do not",
               "agree with CBS titles.\nUpdate the table coding with the shiny",
               "application edit_ts_code.")
  result1 <- expect_warning(get_ts(id, ts_code, refresh = FALSE, 
                          min_year = 2015, raw_cbs_dir = raw_cbs_dir, 
                          frequencies = NA), msg)
  #result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, 
  #                                 min_year = 2015, raw_cbs_dir = raw_cbs_dir, 
  #                                 frequencies = NA), msg)
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})