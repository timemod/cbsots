library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82602NED")

# 
# In this test we check the refresh and min_year arguments. Note that we cannot 
# compare the results with expected results, because the table data is downloaded
# from the CBS and the table may change over time.
#

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8") 

ts_code <- readRDS("tscode/tscode2.rds")

source("utils/check_ts_table.R")
source("utils/read_match_report.R")


id <- "82602NED"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  msg <- paste0("Imperfect matches found for dimension Topic.\n",
                "Check match_reports/82602NED_Topic.xlsx.")
  expect_warning(
    ts_code_new <- update_tables(ts_code, ids = id),
    msg
  )
  
  result1 <- expect_silent(get_ts(id, ts_code_new, refresh = FALSE))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expect_known_value(read_match_report(ts_code, id),
                     file = file.path("expected_output", 
                                      paste0(id, "_match_report.rds")))
})