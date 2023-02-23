library(cbsots)
library(testthat)

rm(list = ls())


options(encoding = "native.enc")

ts_code_file_old <- "tscode/tscode_82595NED_1.rds"

ts_code <- readRDS(ts_code_file_old)

#edit_ts_code(ts_code_file_old)

test_that("fill 84098NED from 82595NED", {
  
  warnings <- capture_warnings(
    tscodes_new <- fill_tables_from_table(ts_code, ids = "84098NED", 
                                        base_id = "82595NED")
  )
  
  expect_known_value(tscodes_new, "expected_output/fill_table_2.rds")
  
  expected_warnings <- c(
    paste0("Imperfect matches found for dimension InstitutioneleSectoren.\n",
           "Check match_reports/82595NED_84098NED_InstitutioneleSectoren.xlsx."),
    paste0("Imperfect matches found for dimension NietGeconsolideerdGeconsolideerd.\n",
           "Check match_reports/82595NED_84098NED_NietGeconsolideerdGeconsolideerd.xlsx."))
  
  expect_identical(warnings, expected_warnings)
  
  # tscodes_old <- readRDS("expected_output/fill_table_2.rds")
  # tscodes_old <- cbsots:::convert_ts_code(tscodes_old)
  # all.equal(tscodes_new, tscodes_old)
  
  if (FALSE) {

    ts_code_file_tmp <- tempfile()
    saveRDS(tscodes_new, ts_code_file_tmp)
    
    edit_ts_code(ts_code_file = ts_code_file_tmp)
    
    unlink(ts_code_file_tmp)
  }
})
