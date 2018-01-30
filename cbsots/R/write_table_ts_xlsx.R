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

#' Writes the timeseries returned by function \code{\link{get_ts}} to
#' an Excel file.
#' 
#' @param x the \code{table_ts} object returned by function \code{\link{get_ts}}
#' @param file a filename
#' @param rowwise a logical value: should the timeseries be written rowwise?
#' @param ... other arguments passed to \code{\link[regts]{write_ts_xlsx}}
#' @importFrom xlsx createWorkbook
#' @importFrom xlsx createSheet
#' @importFrom xlsx addDataFrame
#' @importFrom xlsx autoSizeColumn
#' @importFrom xlsx autoSizeColumn
#' @importFrom xlsx createFreezePane
#' @importFrom xlsx saveWorkbook
#' @importFrom regts write_ts_sheet
#' @export
write_table_ts_xlsx  <- function(x, file, rowwise = TRUE, ...) {
  
  if (!inherits(x, "table_ts")) {
    stop("Argument x is not a table_ts object")
  }
  
  sheet_names <- c(Y = "annual", Q = "quarterly", M = "monthly")
  
  wb <- createWorkbook()
  for (freq in names(x)[-1]) {
    sheet_name <- sheet_names[freq]
    sheet <- createSheet(wb, sheet_name)
    label_option <- if (rowwise) "after"else "no"
    write_ts_sheet(x[[freq]], sheet, ...)
  }
  sheet <- createSheet(wb, "ts_namen")
  addDataFrame(x$ts_namen, sheet, row.names = FALSE)
  autoSizeColumn(sheet, seq_len(ncol(x$ts_namen)))
  createFreezePane(sheet, 2, 1)
  
  saveWorkbook(wb, file)
  
  return(invisible(NULL))
}