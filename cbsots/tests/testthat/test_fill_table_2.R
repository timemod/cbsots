library(cbsots)
library(testthat)

rm(list = ls())

context("fill_tables_from_table (2)")

ts_code_file_old <- "tscode/tscode_82595NED_1.rds"

ts_code <- readRDS(ts_code_file_old)

#edit_ts_code(ts_code_file_old)

test_that("fill 84098NED from 82595NED", {
  
  tscodes_new <- fill_tables_from_table(ts_code, ids = "84098NED", 
                                        base_id = "82595NED")

  #saveRDS(tscodes_new, "tmp.rds")
  
  expect_known_value(tscodes_new, "expected_output/fill_table_2.rds")
  
  # tscodes_old <- readRDS("expected_output/fill_table_2.rds")
  # tscodes_old <- cbsots:::convert_ts_code(tscodes_old)
  # all.equal(tscodes_new, tscodes_old)
  
  if (FALSE) {

    print(tscodes_new$table_code$`83935NED`)

    ts_code_file_tmp <- tempfile()
    saveRDS(tscodes_new, ts_codes_file_tmp)
    
    edit_ts_code(ts_code_file = ts_code_file_tmp)
    
    unlink(ts_code_file_tmp)
  }
})