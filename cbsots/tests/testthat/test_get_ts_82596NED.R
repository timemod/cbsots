library(cbsots)
library(testthat)

rm(list = ls())


# Use UTF-8 endocing, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding 
options(encoding = "UTF-8")

ts_code_file <- "tscode/tscode_82596NED.rds"

#edit_ts_code(ts_code_file)

ts_code <- readRDS(ts_code_file)

source("utils/check_ts_table.R")

id <- "82596NED"

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  expect_silent(result1 <- get_ts(id, ts_code, refresh = FALSE, 
                                  min_year = 2015, raw_cbs_dir = raw_cbs_dir, 
                                  frequencies = NA))
  
  t <- system.time(
    check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  )
  
  expect_true(check)
})
