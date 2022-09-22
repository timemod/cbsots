library(cbsots)
library(testthat)

rm(list = ls())

new_ids <- "84098NED"
base_id <- "82595NED"

update_expected <- FALSE

title <- paste0(base_id, "_", new_ids)

context(paste0("fill_tables_from_table (", title, ")"))

options(encoding = "native.enc")

ts_code_file_old <- "tscode/tscode_iris_jan2018.rds"

ts_code <- readRDS(ts_code_file_old)

source("utils/read_match_report.R")

#edit_ts_code(ts_code_file_old)

test_that("fill 84098NED from 82595NED", {
  
  warnings <- capture_warnings(
    tscodes_new <- fill_tables_from_table(ts_code, ids = new_ids,
                                          base_id = base_id)
  )
  
  expect_known_output(warnings, print = TRUE,
                      file = file.path("expected_output",
                                       paste0(title , "_warnings.txt")),
                      update = update_expected)
 
  expect_known_value(tscodes_new, 
                     file = file.path("expected_output",
                           paste0("fill_table_", title , ".rds")),
                     update = update_expected)
  
  for (id in new_ids) {
    expect_known_value(read_match_report(tscodes_new, id, base_id),
                       file = file.path("expected_output", 
                                      paste0(title, "_match_report.rds")),
                       update = update_expected)
  }
  # tscodes_old <- readRDS("expected_output/fill_table_3.rds")
  # tscodes_old <- cbsots:::convert_ts_code(tscodes_old)
  # print(all.equal(tscodes_new, tscodes_old))
  
  if (FALSE) {
    
    ts_code_file_tmp <- tempfile()
    saveRDS(tscodes_new, ts_code_file_tmp)

    edit_ts_code(ts_code_file = ts_code_file_tmp)

  }
})
