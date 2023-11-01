library(shinytest2)
library(cbsots)

ts_code_file <- "tscode/tscode_test_shiny.rds"

app <- cbsots:::create_shiny_app(ts_code_file = ts_code_file)

app <- AppDriver$new(app, name = "test")

record_test(app)
