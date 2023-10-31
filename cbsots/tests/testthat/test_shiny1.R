library(shiny)
library(cbsots)
library(testthat)
library(tictoc)

ts_code_file <- "tscode/tscode_test_shiny.rds"

tic("total test")


test_that("selecting and deselecting variables and table order", {
  app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                   debug = FALSE)
  
  testServer(app, {
    #print(values$ts_code$`83667NED`)
    table_id <- "83667NED"
    # the first time the event will be ignored
    session$setInputs(table_desc = "")
    expect_true(is.na(values$table_id))
    expect_true(is.na(values$dimension))
    
    session$setInputs(table_desc = unname(values$table_descs[table_id]))
    expect_equal(values$table_id, table_id)
    expect_equal(values$dimension, "Topic")
    hot_input <-list(
      table_id = table_id,
      dim = "Topic",
      data = c(
        "Bouwkosten_1"      , TRUE,  "bov"  , "Bouwkosten", 
        "Bouwvergunningen_2", FALSE,  ""    , "Bouwvergunningen", 
        "Bouwkosten_3"      , TRUE,  "bov_i", "Indexcijfers 2012=100 - Bouwkosten", 
        "Bouwvergunningen_4", FALSE, ""     , "Indexcijfers 2012=100 - Bouwvergunningen"
      )
    )
    session$setInputs(hot = hot_input) 
    
    result1 <- values$ts_code$`83667NED`$codes$Topic
    expect_known_value(result1, "expected_output/shiny1_83667NED_Topic.rds")
    
    session$setInputs(table_order = "Selected First")
    result2 <- values$ts_code$`83667NED`$codes$Topic
    expected_result <- result1
    expected_result[, 1:4] <- expected_result[c(1, 3, 2, 4), 1:4]
    expect_equal(result2, expected_result)
    
    hot_input <-list(
      table_id = table_id,
      dim = "Topic",
      data = c(
        "Bouwkosten_1"      , FALSE,  "bov"  , "Bouwkosten", 
        "Bouwkosten_3"      , TRUE,  "bov_i" , "Indexcijfers 2012=100 - Bouwkosten", 
        "Bouwvergunningen_2", FALSE,  ""     , "Bouwvergunningen", 
        "Bouwvergunningen_4", FALSE, ""      , "Indexcijfers 2012=100 - Bouwvergunningen"
      )
    )
    session$setInputs(hot = hot_input)
    
    expected_result[1, "Select"] <- FALSE
    expect_equal(values$ts_code$`83667NED`$codes$Topic,
                 expected_result)
    
    session$setInputs(reorder = 1)
    
    expected_result[, 1:4] <- expected_result[c(2, 1, 3, 4), 1:4]
    expect_equal(values$ts_code$`83667NED`$codes$Topic,
                 expected_result)
  })
})

test_that("deleting and adding tables", {
  
  table_ids_init <- c("81234ned", "83667NED", "83693NED")
  
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
