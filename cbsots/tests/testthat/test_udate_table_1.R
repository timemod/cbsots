library(cbsots)
library(testthat)

rm(list = ls())

context("update_tables")

id <- "83028NED"
ts_code_file_old <- paste0("tscode/tscode_", id, "_old.rds") 
ts_code <- readRDS(ts_code_file_old)

#edit_ts_code(ts_code_file = ts_code_file_old)

test_that(paste("update", id),  {
  
  expect_known_value(ts_code, "expected_output/update_table_1.rds")
  
  msg <- paste("No matching entries found for dimension SITC:\nA018540 - \"2 Grondstoffen, niet",
              "eetbaar, behalve...\", A018591 - \"3 Minerale brandstoffen,",
              "smeermiddele...\", A018678 - \"6 Fabricaten hoofdzakelijk", 
               "gerangschi...\"")
  expect_warning(tscode_new <- update_tables(ts_code, ids = id), msg)
 
  expect_known_value(tscode_new, "expected_output/update_table_2.rds")
  
  if (FALSE) {

    print(tscodes_new$table_code[[id]])

    ts_code_file_tmp <- "tscode/tscode_83935NED_tmp.rds"
    saveRDS(tscodes_new, ts_code_file_tmp)
    edit_ts_code(ts_code_file = ts_code_file_tmp)

    unlink(ts_code_file_tmp)
  }
})