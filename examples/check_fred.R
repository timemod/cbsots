library(cbsots)

rm(list = ls())

source("functions/check_ts_table.R")

overzicht_xlsx <- "overzicht_opendata_fred.xlsx"
overzicht <- read_excel(overzicht_xlsx, col_types = "text")

id <- "70076ned"
id <- "81974NED"
ts_code_file <- "tscode/tscode_fred.rds"

table_code_collection <- readRDS(ts_code_file)

row <- match(tolower(id), tolower(overzicht$id))
naam_kort <- overzicht[row, "naam_kort"]
ts_filename <- file.path("output/fred", paste0(naam_kort, ".rds"))

x <- readRDS(ts_filename)
ret <- check_ts_table(x, id)