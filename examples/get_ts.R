# R-script voor het downloaden van CBS-tabellen met behulp van pakket 
# cbsodataR, en het omzetten van de CBS-data naar tijdreeksen volgens een 
# bepaalde codering.
#
# PARAMETERS:
#  overizcht_xlsx  naam van xls-file met een overzicht van de CBS-tabellen die
#                 gedownload en verwerkt moeten worden

overzicht_xlsx <- "overzicht_opendata.xlsx"
output_dir <- "output"
code_file <- "tscode/tscode.rds"

library(cbsots)
library(regts)
library(readxl)

overzicht <- read_excel(overzicht_xlsx, col_types = "text")

table_codes <- readRDS(code_file)

for (i in seq_len(nrow(overzicht))) {
  with(overzicht[i, ], {
    if (actie) {
      output_xlsx <- file.path(output_dir, paste0(naam_kort, ".xlsx"))
      output_rds <- file.path(output_dir, paste0(naam_kort, ".rds"))
      unlink(c(output_xlsx, output_rds))
      cat(paste0("\nInlezen van de ", naam_lang, "reeksen (CBS-tabel ",
                id, ") ...\n"))
      reeksen <- get_ts(id, table_codes, refresh = as.logical(refresh))
      write_table_ts_xlsx(reeksen, file = output_xlsx, 
                          rowwise = as.logical(output_rowwise))
      saveRDS(reeksen, output_rds)
    }
  })
}
