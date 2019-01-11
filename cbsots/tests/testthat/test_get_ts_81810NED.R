library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 81810NED")

ts_code <- readRDS("tscode/tscode2.rds")

source("utils/check_ts_table.R")

id <- "81810NED"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  # TODO: controleer of de titles inderdaad niet meer overeenstemmen.
  msg <- paste("Titles in code for dimension Topic in table 81810NED do not",
               "agree with CBS titles.\nUpdate the table coding with the shiny",
               "application edit_ts_code.")
  result1 <- expect_warning(get_ts(id, ts_code, refresh = FALSE), msg)
  #result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE), msg)
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})