library(cbsots)
library(testthat)

rm(list = ls())


source("utils/read_match_report.R")

options(encoding = "native.enc") 

id <- "83028NED"
ts_code_file_old <- paste0("tscode/tscode_", id, "_old.rds") 
ts_code <- readRDS(ts_code_file_old)

#edit_ts_code(ts_code_file = ts_code_file_old)

test_that(paste("update", id),  {
  
  expect_known_value(ts_code, "expected_output/update_table_1.rds")
  
  msg <- paste0("Imperfect matches found for dimension SITC.\n",
                "Check match_reports/83028NED_SITC.xlsx.")
  expect_warning(
    tscode_new <- update_tables(ts_code, ids = id),
    msg
  )

  expect_known_value(tscode_new, "expected_output/update_table_2.rds")
  
  expect_known_value(read_match_report(tscode_new, id),
                     file = file.path("expected_output", 
                                      paste0(id, "_match_report.rds")))
  
  if (FALSE) {

    ts_code_file_tmp <- tempfile()
    saveRDS(tscode_new, ts_code_file_tmp)
    edit_ts_code(ts_code_file = ts_code_file_tmp)

    unlink(ts_code_file_tmp)
  }
})
