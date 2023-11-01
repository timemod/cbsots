library(shinytest2)
library(cbsots)

ts_code_file <- "tscode/tscode_test_shiny.rds"

app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file)

app <- AppDriver$new(app, name = "test")

app$set_inputs(table_desc = "83667NED - Bouwvergunningen; aantal, bouwkosten")

hot_input <-list(
  table_id = "83667NED",
  dim = "Topic",
  data = c(
    "Bouwkosten_1"      , TRUE,  "bov"  , "Bouwkosten", 
    "Bouwvergunningen_2", FALSE,  ""    , "Bouwvergunningen", 
    "Bouwkosten_3"      , TRUE,  "bov_i", "Indexcijfers 2012=100 - Bouwkosten", 
    "Bouwvergunningen_4", FALSE, ""     , "Indexcijfers 2012=100 - Bouwvergunningen"
  )
)
app$set_inputs(hot = hot_input, allow_no_input_binding_ = TRUE)

x <- app$get_values(export = "ts_code")
print(x)

