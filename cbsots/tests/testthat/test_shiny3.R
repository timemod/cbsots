rm(list = ls())
library(shiny)
library(cbsots)
library(testthat)

ts_code_file <- tempfile(pattern = "ts_code_", fileext = ".rds")

source("utils/testServer_gereedschap.R")
source("utils/read_match_report.R")

app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                 debug = FALSE, testServer = TRUE)

expect_warning({
  testServer(app, {
    init_app(session)
    
    id <- "81234ned"
    session$setInputs(`new_table-new_table` = 1)
    session$setInputs(`new_table-new_table_desc` = 
                        cbsots:::get_table_description(id, "xxxx"),
                      `new_table-new_table_base_desc` = "")
    session$setInputs(`new_table-new_table_ok` = 1)
    
    expect_equal(values$table_id, id)
    
    expect_equal(values$dimension, "Topic")

    dim <- "BedrijfstakkenBranchesSBI2008"
    session$setInputs(dimension = dim)
    expect_equal(values$dimension, dim)
    
    # Selecteer de eerste reeks
    values$ts_code[[id]]$codes$Topic[1, "Select"] <- TRUE
    values$ts_code[[id]]$codes$Topic[1, "Code"] <- "x"
    values$ts_code[[id]]$codes$BedrijfstakkenBranchesSBI2008[1, "Select"] <- TRUE
    
    ts_code_orig <- values$ts_code
    
    dim <- "Topic"
    d <- ts_code_orig[[id]]$codes[[dim]]
    d$Key <- paste0(d$Key, "_a")
    values$ts_code[[id]]$codes[[dim]] <- d
    
    dim <- "BedrijfstakkenBranchesSBI2008"
    d <- ts_code_orig[[id]]$codes[[dim]]
    d[1:2, "Title"] <- paste(d[1:2, ]$Title , "x")
    values$ts_code[[id]]$codes[[dim]] <- d
    
    session$setInputs(`update_table-update` = 1)
    session$setInputs(`update_table-update_confirmed` = 1)
    session$setInputs(`update_table-accept_warnings` = 1)
   
    expect_equal(values$ts_code, ts_code_orig)
    
    expect_equal(values$dimension, "Topic")

    expect_known_value(read_match_report(values$ts_code, id),
                       file = file.path("expected_output", 
                                        paste0("match_report", "_shiny3_1.rds")))
    
    # TODO: tweede keer updaten, check match reports geen verschil,
    # geen waarschuwingen
    
  })
})


# TODO: update_all_tables testen....