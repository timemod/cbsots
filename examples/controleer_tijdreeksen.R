# Functie controleer_tijdreeksen controleert of de bekende tijdreeksen
# overeenstemmen met de oorspronkelijke CBS-data. De functie geeft een 
# waarschuwing als er verschillen zijn geconstateerd.
#
# INPUT
#   id              CBS open data table (inclusief extensie NED).
#   naam_kort       Korte omschrijving van de tabel, deze bepaalt de 
#                   naam van de outputfile.
#   ruwe_cbs_dir    Naam van de map waarin de gedownloade CBS-tabellen worden
#                   geschreven (standaard is dit "ruwe_cbs_data")
#   output_dir      Naam van de map waarin de gedownloade CBS-tabellen worden
#                   geschreven (standaard is dit "ruwe_cbs_data").
#
# RETURN:  een lijst met een element voor elke tijdreeksnaam.
#          Het element voor tijdreeksnaam bestaat ook weer uit een lijst,
#          met elementen voor elke frequentie (M, Q of Y).
#          Het element voor elke frequentie is ook een lijst, met de volgende
#          elementen:
#               ranges_equal een logical die aangeeft op de periode-ranges
#                            gelijk zijn.
#               data_equal   een logical die aangeeft of de data gelijk zijn.
#               compare      een data.table met daarin de cbs-data, 
#                            de ts-reeksen en het verschil.

controleer_tijdreeksen <- function(id, naam_kort, 
                                   ruwe_cbs_dir = "ruwe_cbs_data",
                                   output_dir = "output") {
  
  cbs_data_file <- file.path(ruwe_cbs_dir, id, "data.csv")
  ts_result_file <- file.path(output_dir, paste0(naam_kort, ".rds"))
  
  ts_data <- readRDS(ts_result_file)
  na_strings <- c("       .", ".")    
  cbs_data <- fread(cbs_data_file, drop = "ID", na.strings = na_strings)
  
  ts_namen <- ts_data$ts_namen$naam
  
  if (any(!sapply(ts_data[-1], FUN = ncol) == length(ts_namen))) {
    stop("Aantal tijdreeksen klopt niet!")
  }
  
  f <- function(ts_naam, ...) {
    ret <- controleer_tijdreeks(ts_naam, ...)
    ranges_equal <- sapply(ret, FUN = function(x) {x$ranges_equal})
    data_equal <- sapply(ret, FUN = function(x) {x$data_equal})
    if (any(!ranges_equal) || any(!data_equal)) {
      warning(paste("Verschillen gevonden voor", ts_naam, "!\n",
                       "Controleer het resultaat"), immediate. = TRUE)
    }
    return(ret)
  }
  return(sapply(ts_namen, FUN = f, ts_data = ts_data, cbs_data = cbs_data, 
                simplify = FALSE))
}

controleer_tijdreeks <- function(ts_naam, ts_data, cbs_data) {
  
  ts_namen <- ts_data$ts_namen
  dimensies <- setdiff(colnames(ts_namen)[2:(ncol(ts_namen) - 1)], "Topic")
  frequenties <- names(ts_data)[-1]
  
  ts_info <- ts_namen[naam == ts_naam]

  # selecteer kolommen die op ts_namen betrekking hebben
  cbs_data <- cbs_data[, c(dimensies, "Perioden", ts_info$Topic), with = FALSE]

  if (length(dimensies) > 0) {
    for (dimensie in dimensies) {
      cbs_data <- cbs_data[eval(as.name(dimensie)) == ts_info[[dimensie]]] 
    }
    cbs_data[, (dimensies) := NULL]
  }
  
  # conversietabel regts-frequenties <-> cbs-frequenties
  freq_table <- c(Y = "JJ", Q = "KW", M = "MM")
  
   vergelijk_reeksen <- function(freq) {
    
    cbs_freq <- freq_table[freq]
    cbs_data <- cbs_data[grepl(cbs_freq, Perioden)]
    
    pattern <- if (freq == "M") cbs_freq else paste0(cbs_freq, "0*") 
    replacement <- if (freq == "Y") "" else freq
    per_cbs <- sub(pattern, replacement, cbs_data$Perioden)
    data_cbs <- as.numeric(cbs_data[[2]]) 
   
    ts_ts <- ts_data[[freq]][, ts_naam]
    ts_df <- as.data.frame(ts_ts)
    per_ts <- rownames(ts_df)
    data_ts <- as.numeric(ts_df[[1]])

    ranges_equal <- isTRUE(identical(per_cbs, per_ts))
    data_equal   <- isTRUE(all.equal(data_cbs, data_ts))
    
    compare <- data.table(per_cbs, per_ts, data_cbs, data_ts, 
                          diff = data_ts - data_cbs)
    
    return(list(ranges_equal = ranges_equal, data_equal = data_equal,
                compare = compare))
  }
  
  return(sapply(frequenties, FUN = vergelijk_reeksen, simplify = FALSE))
}
