rm(list = ls())

library(cbsots)
library(readxl)

overzicht_xlsx <- "overzicht_opendata_fred.xlsx"
output_rds <- "tscodes/tscodes_fred.rds"
tijdreekscode_dir <- "tscodes_xlsx_fred"

overzicht <- read_excel(overzicht_xlsx, col_types = "text")

table_ids <- overzicht$id
xlsx_files <- file.path(tijdreekscode_dir, paste0(overzicht$naam_kort, 
                                                  "_code.xlsx"))
names(xlsx_files) <- table_ids


#xlsx_files <- xlsx_files[1:2]

tables <- read_ts_code_xlsx(xlsx_files) 

saveRDS(tables, output_rds)
