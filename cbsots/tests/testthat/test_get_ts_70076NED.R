library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 70076NED")

ts_code <- readRDS("tscode/tscode2.rds")

source("utils/check_ts_table.R")

id <- "70076ned"

test_that(paste(id, "no refresh"), {
   
   raw_cbs_dir <- "raw_cbs_data"
   
   result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE))
   
   check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
   expect_true(check$equal)
})

test_that(paste(id, "refresh"), {
  
  raw_cbs_dir <- tempdir()
  
  result1 <- expect_output(get_ts(id, ts_code, refresh = TRUE, min_year = 2016,
                                  frequencies = "Y", raw_cbs_dir = raw_cbs_dir))
  
  expect_equal_to_reference(result1, file.path("expected_output", "70076NED.rds"))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check$equal)
})
