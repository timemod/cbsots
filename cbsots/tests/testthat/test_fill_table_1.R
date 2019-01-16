library(cbsots)
library(testthat)

rm(list = ls())

context("fill_tables_from_table (1)")

options(encoding = "native.enc")

ts_code_file_old <- "tscode/tscode_81974NED.rds"

#edit_ts_code(ts_code_file_old)

ts_code <- readRDS(ts_code_file_old)


test_that("fill 83935NED from 81974NED", {
  
  tscodes_new <- fill_tables_from_table(ts_code, ids = "83935NED", 
                                        base_id = "81974NED")

  expect_known_value(tscodes_new, "expected_output/fill_table_1.rds")
  
  # tscodes_old <- readRDS("expected_output/fill_table_1.rds")
  # tscodes_old <- cbsots:::convert_ts_code(tscodes_old)
  # all.equal(tscodes_new, tscodes_old)
  
  if (FALSE) {

    ts_code_file_tmp <- tempfile()
    saveRDS(tscodes_new, ts_code_file_tmp)
    
    edit_ts_code(ts_code_file = ts_code_file_tmp)
    
    unlink(ts_code_file_tmp)
  }
})