library(cbsots)
library(testthat)

rm(list = ls())

id <- "83262NED"

update_expected <- FALSE

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8") 

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  expect_silent(result1 <- get_ts(id, ts_code, download = FALSE))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expected_output_file <- sprintf("expected_output/%s.rds", id)
  expect_known_output(result1, expected_output_file)
  
  expect_silent(result2 <- get_ts(id, ts_code, download = FALSE,
                                  frequencies = "H"))
  expect_equal(result2$H, result1$H)
  expect_equal(names(result2), c("H", "ts_names"))
  
  expect_warning(result3 <- get_ts(id, ts_code, download = FALSE,
                                  frequencies = "hq"),
                 "Frequencies Q not present in table")
  expect_equal(result2, result3)
  
  expect_error(get_ts(id, ts_code, download = FALSE, frequencies = "Z"),
               "Unknown frequencies Z specified")
})


