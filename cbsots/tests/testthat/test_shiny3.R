rm(list = ls())
library(shiny)
library(cbsots)
library(testthat)

source("utils/testServer_gereedschap.R")
source("utils/read_match_report.R")

ts_code_file_ok <- tempfile(pattern = "ts_code_", fileext = ".rds")
ts_code_file_err <- tempfile(pattern = "ts_code_", fileext = ".rds")

test_that("preparations", {
  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file_err,
                                   debug = FALSE, testServer = TRUE)
  
  testServer(app, {
    # add a new table
    id <- "81234ned"
    session$setInputs(`new_table-new_table` = 1)
    session$setInputs(`new_table-new_table_desc` = 
                        cbsots:::get_table_description(id, "xxxx"),
                      `new_table-new_table_base_desc` = "")
    session$setInputs(`new_table-new_table_ok` = 1)
    
    expect_equal(values$table_id, id)
    expect_equal(values$dimension, "Topic")
    
    # select the first timeseries
    values$ts_code[[id]]$codes$Topic[1, "Select"] <- TRUE
    values$ts_code[[id]]$codes$Topic[1, "Code"] <- "x"
    values$ts_code[[id]]$codes$BedrijfstakkenBranchesSBI2008[1, "Select"] <- TRUE
    
    # open een tweede tabel:
    id <- "83667NED"
    session$setInputs(`new_table-new_table` = 1)
    session$setInputs(`new_table-new_table_desc` = 
                        cbsots:::get_table_description(id, "xxxx"),
                      `new_table-new_table_base_desc` = "")
    session$setInputs(`new_table-new_table_ok` = 1)
    expect_equal(values$table_id, id)
    expect_equal(values$dimension, "Topic")
    
    # select the first timeseries
    values$ts_code[[id]]$codes$Topic[2, "Select"] <- TRUE
    values$ts_code[[id]]$codes$Topic[2, "Code"] <- "x"
    values$ts_code[[id]]$codes$AardWerkzaamheden[2, "Select"] <- TRUE
    values$ts_code[[id]]$codes$Gebouwbestemming[2, "Select"] <- TRUE
  
    session$setInputs(dimension = "AardWerkzaamheden")
    session$setInputs(table_order = "Selected First")
    
    expect_known_value(values$ts_code[[id]]$codes$AardWerkzaamheden,
                       file.path("expected_output", 
                                  paste0("shiny3_AardWerkzaamheden.rds")))
    
    # save ts_code and copy to ts_file_ok
    session$setInputs(save = 1)
    stopifnot(file.copy(ts_code_file_err, ts_code_file_ok, overwrite = TRUE))
    
    # Now modify Keys and Titles for both tables
    ts_code_orig <- values$ts_code
    id <- "81234ned"
    dim <- "Topic"
    
    
    d <- ts_code_orig[[id]]$codes[[dim]]
    d$Key <- paste0(d$Key, "_a")
    d$OrigKeyOrder <-  paste0(d$OrigKeyOrder, "_a")
    values$ts_code[[id]]$codes[[dim]] <- d
    values$ts_code[[id]]$short_title <- paste(values$ts_code[[id]]$short_title,
                                              "CBS")
    dim <- "BedrijfstakkenBranchesSBI2008"
    d <- ts_code_orig[[id]]$codes[[dim]]
    d[1:2, "Title"] <- paste(d[1:2, ]$Title , "x")
    values$ts_code[[id]]$codes[[dim]] <- d
    
    id <- "83667NED"
    dim <- "AardWerkzaamheden"
    d <- ts_code_orig[[id]]$codes[[dim]]
    d[1, "Key"] <- "xxx"
    d[2, "OrigKeyOrder"] <- "xxx"
    values$ts_code[[id]]$codes[[dim]] <- d
    
    session$setInputs(save = 1)
  })
})

test_that("update a single table", {
  
  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file_err,
                                   debug = FALSE, testServer = TRUE)
  
  # TODO: remove match reports
  
  ts_code_ok <- readRDS(ts_code_file_ok)
  
  expect_warning({
    testServer(app, {
      init_app(session)

      ts_code_init <- values$ts_code
      
      # selecteer tweede tabblad      
      id <-  "81234ned"
      session$setInputs(table_desc =  cbsots:::get_table_description(id, "xxxx"))
      
      dim <- "BedrijfstakkenBranchesSBI2008"
      session$setInputs(dimension = dim)
      expect_equal(values$table_id, id)
      expect_equal(values$dimension, dim)
      
      expect_true(endsWith(values$ts_code[[id]]$short_title, " CBS"))
        
      session$setInputs(`update_table-update` = 1)
      session$setInputs(`update_table-update_confirmed` = 1)
      session$setInputs(`update_table-accept_warnings` = 1)
 
      expect_false(endsWith(values$ts_code[[id]]$short_title, " CBS"))
      
      expect_equal(values$ts_code[[id]], ts_code_ok[[id]])
     
      expect_equal(values$dimension, "Topic")
      
      expect_known_value(read_match_report(values$ts_code, id),
                         file = file.path("expected_output", 
                                          paste0("shiny3_match_report_", id, "_1.rds")))
      
      session$setInputs(`update_table-update` = 1)
      session$setInputs(`update_table-update_confirmed` = 1)
      
      expect_known_value(read_match_report(values$ts_code, id),
                         file = file.path("expected_output", 
                                          paste0("shiny3_match_report_", id, "_2.rds")))
      
      id <- "83667NED"
      expect_equal(values$ts_code[[id]], ts_code_init[[id]])
      
      session$setInputs(table_desc =  cbsots:::get_table_description(id, "xxxx"))
      
      session$setInputs(`update_table-update` = 1)
      session$setInputs(`update_table-update_confirmed` = 1)
      expect_equal(values$ts_code[[id]], ts_code_init[[id]])
      
      session$setInputs(`update_table-accept_warnings` = 1)
      
      expect_known_value(read_match_report(values$ts_code, id),
                         file = file.path("expected_output", 
                                          paste0("shiny3_match_report_", id, ".rds")))
  
      expect_equal(values$ts_code, ts_code_ok)
    })
  })
})

test_that("update all tables", {
  
  # TODO: remove match reports
  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file_err,
                                   debug = FALSE, testServer = TRUE)
  
  ts_code_ok <- readRDS(ts_code_file_ok)
  
  expect_warning({
    testServer(app, {
      init_app(session)
      
      ts_code_init <- values$ts_code
      
      session$setInputs(`update_all_tables-update` = 1)
      session$setInputs(`update_all_tables-update_confirmed` = 1)
      expect_equal(values$ts_code, ts_code_init)
      session$setInputs(`update_all_tables-accept_warnings` = 1)
      expect_equal(values$ts_code, ts_code_ok)
      
      expect_true(is.na(values$dimension))
      
      id <-  "81234ned"
      expect_known_value(read_match_report(values$ts_code, id),
                         file = file.path("expected_output", 
                                          paste0("shiny3_match_report_", id, "_1.rds")),
                         update = FALSE)
      
      id <- "83667NED"
      expect_known_value(read_match_report(values$ts_code, id),
                         file = file.path("expected_output", 
                                          paste0("shiny3_match_report_", id, ".rds")),
                         update = FALSE)
    })
  })
})

