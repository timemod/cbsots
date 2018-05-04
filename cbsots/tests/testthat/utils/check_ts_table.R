# Functie check_ts_table controleert of de bekende tijdreeksen
# overeenstemmen met de oorspronkelijke CBS-data. De functie geeft een 
# waarschuwing als er verschillen zijn geconstateerd.
#
# INPUT
#   id              CBS open data table (inclusief extensie NED).
#   name_kort       Korte omschrijving van de tabel, deze bepaalt de 
#                   name van de outputfile.
#   ruwe_cbs_dir    name van de map waarin de gedownloade CBS-tabellen worden
#                   geschreven (standaard is dit "ruwe_cbs_data")
#   output_dir      name van de map waarin de gedownloade CBS-tabellen worden
#                   geschreven (standaard is dit "ruwe_cbs_data").
#
# RETURN:  een lijst met een element voor elke tijdreeksname.
#          Het element voor tijdreeksname bestaat ook weer uit een lijst,
#          met elementen voor elke frequentie (M, Q of Y).
#          Het element voor elke frequentie is ook een lijst, met de volgende
#          elementen:
#               ranges_equal een logical die aangeeft op de periode-ranges
#                            gelijk zijn.
#               data_equal   een logical die aangeeft of de data gelijk zijn.
#               compare      een data.table met daarin de cbs-data, 
#                            de ts-reeksen en het verschil.


library(data.table)

check_ts_table <- function(x, id, raw_cbs_dir = "raw_cbs_data") {
  
  cbs_data_file <- file.path(raw_cbs_dir, id, "data.csv")

  cbs_data <- data.table::fread(cbs_data_file, drop = "ID")
  
  # The raw cbs data downloaded with cbsodataR versions prior to 0.3 
  # contained strings such as "       ." for NA values. Therefore replace 
  # them with NA_character_. Note that we cannot use argument na.string of
  # function fread, because fread does not support NA strings with spaces
  # any more.
  na_strings_old <- c("       .", ".", "       -")  
  check_na_strings<- function(x) {
    if (is.character(x)) {
      return(ifelse(x %in% na_strings_old, NA_character_, x))  
    } else {
      return(x)
    }
  }
  cbs_data <- as.data.table(lapply(cbs_data, FUN = check_na_strings))
  
  
  ts_names <- x$ts_names$name

  frequencies <- setdiff(names(x), c("ts_names", "meta"))
  
  if (any(!sapply(x[frequencies], FUN = ncol) == length(ts_names))) {
    stop("The number of timeseries is not correct.")
  }
  
  equal <- TRUE
  
  f <- function(ts_name, ...) {
    ret <- controleer_tijdreeks(x, ts_name, ...)
    ranges_equal <- sapply(ret, FUN = function(x) {x$ranges_equal})
    data_equal <- sapply(ret, FUN = function(x) {x$data_equal})
    if (any(!ranges_equal) || any(!data_equal)) {
      equal <- FALSE
      warning(paste("Differences found for series", ts_name, "!\n",
                       "Check the result"), immediate. = TRUE)
    }
    return(ret)
  }
  
  return(c(list(equal = equal), 
           sapply(ts_names, FUN = f, ts_data = x, cbs_data = cbs_data, 
                  simplify = FALSE)))
}

controleer_tijdreeks <- function(x, ts_name, ts_data, cbs_data) {
  
  ts_names <- as.data.table(ts_data$ts_names)
  dimensies <- setdiff(colnames(ts_names)[2:(ncol(ts_names) - 1)], "Topic")
  frequenties <-  setdiff(names(x), c("ts_names", "meta"))
  
  ts_info <- ts_names[name == ts_name]

  # selecteer kolommen die op ts_names betrekking hebben
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
   
    ts_ts <- ts_data[[freq]][, ts_name]
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
