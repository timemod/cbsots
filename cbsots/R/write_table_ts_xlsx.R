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
#' @param ... other arguments passed to 
#' \code{\link[regts:write_ts_xlsx-slash-write_ts_sheet]{write_ts_xlsx}}
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx freezePane
#' @importFrom openxlsx setColWidths
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' 
#' @importFrom regts write_ts_sheet
#' @export
write_table_ts_xlsx  <- function(x, file, rowwise = TRUE, ...) {
  
  if (!inherits(x, "table_ts")) {
    stop("Argument x is not a table_ts object")
  }
  
  freq_sheet_names <- c(Y = "annual", H = "semi-annual",
                        Q = "quarterly", M = "monthly")
  
  frequencies <- intersect(names(x), names(freq_sheet_names))
  
  if (length(frequencies) == 0) {
    stop("Argument x does not have any frequency component")
  }
  
 
  wb <- createWorkbook()
  for (freq in frequencies) {
    sheet_name <- freq_sheet_names[freq]
    addWorksheet(wb, sheet_name)
    label_option <- if (rowwise) "after" else "no"
    write_ts_sheet(x[[freq]], wb, sheet_name, rowwise = rowwise, 
                   labels = label_option, ...)
  }
  
  sheet_name <- "ts_names"
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, x$ts_names, rowNames = FALSE)
  setColWidths(wb, sheet_name, 1:ncol(x$ts_names), widths = "auto")
  freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
  
  # write meta data
  meta <- x$meta
  if (!is.null(meta)) {
    for (name in names(meta)) {
      data <- meta[[name]]
      if (length(data) == 0) next
      sheet_name <- paste0("meta_data_", name)
      if (nchar(sheet_name) > 31) {
        sheet_name <- substr(sheet_name, 1, 31)
      }
      addWorksheet(wb,  sheet_name)
      writeData(wb, sheet_name, data, rowNames = FALSE)
      setColWidths(wb, sheet_name, 1:ncol(data), widths = "auto")
      freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
    }
  }
  
  minWidth_old <- options("openxlsx.minWidth")[[1]]
  options("openxlsx.minWidth" = 8.43)
  tryCatch({
    result <- saveWorkbook(wb, file, overwrite = TRUE, returnValue = TRUE)
    if (!isTRUE(result)) stop(result$message, call. = FALSE)
  }, finally = {
    options("openxlsx.minWidth" = minWidth_old)
  })
  
  
  return(invisible(NULL))
}