# Functie output_tijdreeksen schrijft de tijdreeksen naar een rds-
# en naar een Excel-file.
#
# INPUT
#   reeksen         de return-waarde van functie cbs2ts (een lijst met 
#                   tijdreeksnamen en tijdreeksen). 
#   naam_kort       Korte omschrijving van de tabel, deze bepaalt de 
#                   naam van de outputfile.
#   naam_lang       Uitgebreidere omschrijving van de tabel, wordt gebruikt voor
#                   boodschappen naar het scherm.
#   output_dir      Naam van de map waarin de bestanden worden weggeschreven
#                   geschreven (standaard is dit "ruwe_cbs_data")
#   rowwise         Een logical die aangeeft of de tijdreeksdata rijgewijs
#                   moet worden weggeschreven
#   write_xlsx      Een logical die aangeeft of de tijdreeksen naar een 
#                   Excel-file moeten worden weggeschreven.
#
# RETURN:  NULL
#
# De tijdreeksen worden geschreven naar rds-file <output_dir>/<naam_kort>.rds
# en Excel file <output_dir>/<naam_kort>.xlsx.
#
output_tijdreeksen <- function(reeksen,  naam_kort, naam_lang,
                               output_dir = "output", rowwise = TRUE, 
                               write_xlsx = TRUE) {
  
  output_rds <- file.path(output_dir, paste0(naam_kort, ".rds"))
  output_xlsx <- file.path(output_dir, paste0(naam_kort, ".xlsx"))
  
  cat(paste("Wegschrijven van", naam_lang, "reeksen naar", output_rds, "...\n"))
  saveRDS(reeksen, output_rds)
  
  if (write_xlsx) {
    cat(paste("Wegschrijven van", naam_lang, "reeksen naar", output_xlsx, 
              "...\n"))
    sheet_names <- c(Y = "annual", Q = "quarterly", M = "monthly")
    wb <- createWorkbook()
    for (freq in names(reeksen)[-1]) {
      sheet_name <- sheet_names[freq]
      sheet <- createSheet(wb, sheet_name)
      label_option <- if (rowwise) "after"else "no"
      write_ts_sheet(reeksen[[freq]], sheet, rowwise = rowwise,
                     labels = label_option)
    }
    sheet <- createSheet(wb, "ts_namen")
    addDataFrame(reeksen$ts_namen, sheet, row.names = FALSE)
    autoSizeColumn(sheet, seq_len(ncol(reeksen$ts_namen)))
    createFreezePane(sheet, 2, 1)
    saveWorkbook(wb, output_xlsx)
  }
  
  return(invisible(NULL))
}