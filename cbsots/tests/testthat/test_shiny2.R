rm(list = ls())
library(shiny)
library(cbsots)
library(testthat)

ts_code_file <- "tscode/tscode_iris_jan2018.rds"
new_id <- "84098NED"
base_id <- "82595NED"

source("utils/testServer_gereedschap.R")
source("utils/read_match_report.R")

test_that("create new table with base table", {
  
  # remove the match report that will be created
  pattern <- paste0("^", base_id, "_", new_id, ".+\\.xlsx$")
  files <- list.files(path = "match_reports", pattern = pattern,
                      full.names = TRUE)
  stopifnot(all(file.remove(files)))

  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                   debug = FALSE, testServer = TRUE)
  
  expect_warning({ # for some reasons the job still issues warnings
    testServer(app, {
      init_app(session)
    
      session$setInputs(`new_table-new_table` = 1)
      session$setInputs(`new_table-new_table_desc` = 
                          cbsots:::get_table_description(new_id, "xxxx"),
                        `new_table-new_table_base_desc` = 
                          cbsots:::get_table_description(base_id, "xxx"))
      session$setInputs(`new_table-new_table_ok` = 1)
      expect_null(tblcod_new())
      session$setInputs(`new_table-warnings_ok` = 1)
      expect_equal(tblcod_new()$id, new_id)
      print(values$table_ids)
      expect_true(new_id %in% values$table_ids)
      expect_equal(values$table_id, new_id)
      expect_equal(input$table_desc, values$table_descs[new_id])
      
      expect_known_value(read_match_report(values$ts_code, new_id, base_id),
                         file = file.path("expected_output", 
                                          paste0(base_id, "_", new_id, 
                                                 "_match_report.rds")),
                         update = FALSE)
    })
  })
})