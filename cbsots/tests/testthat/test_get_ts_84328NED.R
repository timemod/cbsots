library(cbsots)
library(testthat)

rm(list = ls())

id <- "84328NED"

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8") 

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  msg <- paste("Duplicate keys in cbs meta data for dimension Topic in",
               "table 84328NED:\n'KapitaalgoederenvoorraadEindbalans_1'\\.")
  expect_warning(result1 <- get_ts(id, ts_code, download = FALSE),
                 msg)
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})
