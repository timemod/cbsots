# R-script voor het downloaden van CBS-tabellen met behulp van pakket 
# cbsodataR, en het omzetten van de CBS-data naar tijdreeksen volgens een 
# bepaalde codering.
#
# PARAMETERS:
#  overizcht_xlsx  naam van xls-file met een overzicht van de CBS-tabellen die
#                 gedownload en verwerkt moeten worden

overzicht_xlsx <- "overzicht_opendata_fred.xlsx"
output_dir <- "output/fred"
code_file <- "tscode/tscode_fred.rds"

cpblib::use_cpblib()
library(cbsots)
library(regts)
library(readxl)

suppressPackageStartupMessages(library("data.table"))

options(java.parameters = "-Xmx8000m")

overzicht <- read_excel(overzicht_xlsx, col_types = "text")

table_codes <- readRDS(code_file)

for (i in seq_len(nrow(overzicht))) {
  with(overzicht[i, ], {
    if (actie) {
      unlink(file.path(output_dir, paste0(naam_kort, "*")))
      cat(paste0("\nInlezen van de ", naam_lang, "reeksen (CBS-tabel ",
                id, ") ...\n"))
      reeksen <- get_ts(id, table_codes, download = as.logical(download))
      output_xlsx <- file.path(output_dir, paste0(naam_kort, ".xlsx"))
      write_table_ts_xlsx(reeksen, file = output_xlsx, 
                          rowwise = as.logical(output_rowwise))
      output_rds <- file.path(output_dir, paste0(naam_kort, ".rds"))
      saveRDS(reeksen, output_rds)
    }
  })
}
