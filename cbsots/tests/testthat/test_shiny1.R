library(shiny)
library(cbsots)
library(testthat)
library(tictoc)

ts_code_file <- "tscode/tscode_test_shiny.rds"

tic("total test")



table_ids_init <- c("81234ned", "83667NED", "83693NED")

test_that("deleting and adding tables", {
  
  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                   debug = FALSE)
  
  testServer(app, {
    expect_true(is.na(values$table_id))
    table_ids <- names(values$ts_code)
    expect_equal(names(values$ts_code), table_ids_init)
    expect_equal(values$table_ids, table_ids_init)
    
    session$setInputs(`delete_table-delete` = 1)
    delete_id <- "81234ned"
    delete_table_desc <- values$table_descs[delete_id] 
    session$setInputs(`delete_table-delete_desc` = delete_table_desc)
    session$setInputs(`delete_table-delete_ok` = 1)
    expect_equal(values$table_ids, table_ids_init)
    session$setInputs(`delete_table-delete_confirmed` = 1)
    expect_equal(delete_table_id(), delete_id)
    expect_equal(values$table_ids, setdiff(table_ids_init, delete_id))
    
    # Add the table again
    session$setInputs(`new_table-new_table` = 1)
    session$setInputs(`new_table-new_table_desc` = 
                        cbsots:::get_table_description(delete_id, "xxxx"),
                      `new_table-new_table_base_desc` = "")
    session$setInputs(`new_table-new_table_ok` = 1)
    expect_equal(tblcod_new()$id, delete_id)
    expect_equal(values$table_ids, table_ids_init)
    
    # And delete again. Note that the the delete table desc may have changed
    # because of a change of the table title
    delete_table_desc <- values$table_descs[delete_id]
    session$setInputs(`delete_table-delete` = 1)
    session$setInputs(`delete_table-delete_desc` = delete_table_desc)
    session$setInputs(`delete_table-delete_ok` = 1)
    session$setInputs(`delete_table-delete_confirmed` = 1)
    expect_equal(delete_table_id(), delete_id)
    expect_equal(values$table_ids, setdiff(table_ids_init, delete_id))
    
    # and now add the table again
    session$setInputs(`new_table-new_table` = 1)
    session$setInputs(`new_table-new_table_desc` = 
                        cbsots:::get_table_description(delete_id, "xxxx"),
                      `new_table-new_table_base_desc` = "")
    session$setInputs(`new_table-new_table_ok` = 1)
    expect_equal(tblcod_new()$id, delete_id)
    expect_equal(values$table_ids, table_ids_init)
  })
})