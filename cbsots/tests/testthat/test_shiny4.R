rm(list = ls())
library(shiny)
library(testthat)
library(cbsots)

ts_code_file_orig <- "tscode/tscode_test_shiny.rds"
ts_code_file <- tempfile(pattern = "ts_code_", fileext = ".rds")
stopifnot(file.copy(ts_code_file_orig, ts_code_file))
          
source("utils/testServer_gereedschap.R")

app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                 debug = FALSE, testServer = TRUE)

testServer(app, {
  
  init_app(session)
  
  id <- "83667NED" 
  
  table_descs <- values$table_descs
  session$setInputs(table_desc = unname(table_descs[[id]]))
  
  expect_equal(values$table_id, id)
  expect_equal(values$dimension, "Topic")
  
  hot_input <-list(
    table_id = id,
    dim = "Topic",
    data = c(
      "Bouwkosten_1"      , TRUE,  "bov"  , "Bouwkosten", 
      "Bouwvergunningen_2", FALSE,  ""    , "Bouwvergunningen", 
      "Bouwkosten_3"      , TRUE,  "bov_i", "Indexcijfers 2012=100 - Bouwkosten", 
      "Bouwvergunningen_4", FALSE, ""     , "Indexcijfers 2012=100 - Bouwvergunningen"
    )
  )
  session$setInputs(hot = hot_input)
  
  dim <- "Gebouwbestemming"
  session$setInputs(dimension = dim)
  expect_equal(values$dimension, dim)
  
  hot_input <-list(
    table_id = id,
    dim = "Gebouwbestemming",
    data = c(
      "A007232", FALSE, "wown", "Woonsector",
      "A007237", TRUE,  "bgwn", "Bedrijfssector",
      "T001032", FALSE, "",     "Totaal"
    )
  )
  session$setInputs(hot = hot_input)
  
  session$setInputs(dimension_order = c("Gebouwbestemming", "Topic", 
                                        "AardWerkzaamheden"))
  
  
  gebouwbestemming1 <- values$ts_code$`83667NED`$codes$Gebouwbestemming
  expect_equal(gebouwbestemming1$Key, c("A007232", "A007237", "T001032"))
  
  id <- "83693NED"
  session$setInputs(table_desc = unname(table_descs[[id]]))
  expect_equal(values$table_id, id)
  expect_equal(values$dimension, "Topic")
  
  # after selecting a new table, the table is no longer reordered
  gebouwbestemming2 <- values$ts_code$`83667NED`$codes$Gebouwbestemming
  expect_equal(gebouwbestemming2$Key, c("A007232", "A007237", "T001032"))
  
  hot_input <- list(
    table_id = id,
    dim = "Topic",
    data = c(
      "Consumentenvertrouwen_1"               , TRUE  , "convrt" , "Consumentenvertrouwen",                  
      "EconomischKlimaat_2"                   , FALSE  , "ek"    , "Economisch klimaat",                     
      "Koopbereidheid_3"                      , TRUE  , "koop"   , "Koopbereidheid",                         
      "EconomischeSituatieLaatste12Maanden_4" , FALSE , ""       , "Economische situatie laatste 12 maanden",
      "EconomischeSituatieKomende12Maanden_5" , FALSE , "xxx"    , "Economische situatie komende 12 maanden"
    )
  )
  session$setInputs(hot = hot_input)

  session$setInputs(save = 1)
  
  #print(values$ts_code)
  expect_known_value(values$ts_code, "expected_output/shiny4_ts_code.rds")

})


app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file,
                                 debug = FALSE, testServer = TRUE)

testServer(app, {
  # now gebouwbestemming should be sorted (when the ts_code is read from the
  # file it is ordered)
  gebouwbestemming <- values$ts_code$`83667NED`$codes$Gebouwbestemming
  expect_equal(gebouwbestemming$Key, c("A007237", "T001032", "A007232"))
})