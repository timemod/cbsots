library(cbsots)
library(testthat)

rm(list = ls())

context("fill_tables_from_table (3)")

ts_code_file_old <- "tscode/tscode_ernest.rds"

ts_code <- readRDS(ts_code_file_old)

new_ids <- c("83357NED", "83361NED")
base_id <- "83186NED"

# edit_ts_code(ts_code_file_old)

test_that("fill 84098NED from 82595NED", {
  
  msg <- 
    paste0("No matching entries found for dimension BedrijfstakkenBranchesSBI2008:\n",
           "305700  - \"B Delfstoffenwinning\"\n",
           "307500  - \"C Industrie\"\n",
           "346600  - \"D Energievoorziening\"\n",
           "348000  - \"E Waterbedrijven en afvalbeheer\"\n",
           "354200  - \"G Handel\"\n",
           "383100  - \"H Vervoer en opslag\"\n",
           "389100  - \"I Horeca\"\n",
           "403300  - \"M Specialistische zakelijke diensten\"\n",
           "410200  - \"N Verhuur en overige zakelijke diensten\"")
  
  expect_warning(tscodes_new <- fill_tables_from_table(ts_code, ids = new_ids,
                                        base_id = "83186NED"), msg)

 
  expect_known_value(tscodes_new, "expected_output/fill_table_3.rds")
  
  # tscodes_old <- readRDS("expected_output/fill_table_3.rds")
  # tscodes_old <- cbsots:::convert_ts_code(tscodes_old)
  # print(all.equal(tscodes_new, tscodes_old))
  
  if (FALSE) {
    
    ts_code_file_tmp <- tempfile()
    saveRDS(tscodes_new, ts_code_file_tmp)

    edit_ts_code(ts_code_file = ts_code_file_tmp)

  }
})