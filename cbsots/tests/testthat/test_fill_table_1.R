library(cbsots)
library(testthat)

rm(list = ls())

context("fill_tables_from_table")

ts_code_file_old <- "tscode/tscode_81974NED.rds"


ts_code <- readRDS(ts_code_file_old)


test_that("fill 83935NED from 81974NED", {
  
  tscodes_new <- fill_tables_from_table(ts_code, ids = "83935NED", 
                                        base_id = "81974NED")

  expect_known_value(tscodes_new, "expected_output/full_table_1.rds")
  
  if (FALSE) {

    print(tscodes_new$table_code$`83935NED`)

    ts_code_file_tmp <- "tscode/tscode_83935NED_tmp.rds"
    saveRDS(tscodes_new, ts_code_file_tmp)
    edit_ts_code(ts_code_file = ts_code_file_tmp)

    unlink(ts_code_file_tmp)
  }
})