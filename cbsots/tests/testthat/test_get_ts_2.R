library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts: compare expected output")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

# 
# In this test we will compare the results with expected output, 
# therefore we use refresh = FALSE, and the files in directory
# raw_cbs_data should be added to the Git repository.
# We should make sure that subsequent calls of get_ts do not
# modify the data.
# 

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "7137shih"
test_that(id, {

  # we don't expect any output because refresh = FALSE and the data is
  # present in directory raw_cbs_data
  result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, min_year = 2015,
                                  include_meta = TRUE))

  # the result of sort is platform dependent (it is actually also dependend on
  # the locale)
  expected_value_file <- file.path("expected_output", paste0(id, "_result_", 
                                   .Platform$OS.type, ".rds"))
  expect_known_value(result1, file = expected_value_file)

  expected_output_file <- file.path("expected_output",
                                    paste0(id, "_print_result_", 
                                           .Platform$OS.type, ".txt"))
  expect_known_output(print(result1), expected_output_file)

  check <- check_ts_table(result1, id)
  expect_true(check)
})