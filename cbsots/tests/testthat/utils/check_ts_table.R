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

check_ts_table <- function(x, id, raw_cbs_dir = "raw_cbs_data",
                           min_year) {
  
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
  
  ts_names <- as.data.table(x$ts_names)
  key_columns <- grep("_Key$", colnames(ts_names)[2:ncol(ts_names) - 1], 
                      value = TRUE)
  dimensions <- setdiff(sub("_Key$", "", key_columns), "Topic")
  frequencies <-  setdiff(names(x), c("ts_names", "meta"))
  
  # select topic columns 
  cbs_data <- cbs_data[ , c(dimensions, "Perioden", ts_names$Topic_Key), 
                        with = FALSE]
  
  if (!missing(min_year)) {
    years <- sub("^(\\d+).*", "\\1", cbs_data$Perioden)
    years <- as.numeric(years)
    cbs_data <- cbs_data[years >= min_year]
  }
  
  # split data in frequencies
  
  # conversietabel regts-frequenties <-> cbs-frequenties
  freq_table <- c(Y = "JJ", H = "HJ", Q = "KW", M = "MM")
  
  select_freq <- function(freq) {
    cbs_freq <- freq_table[freq]
    freq_data <- cbs_data[grepl(cbs_freq, Perioden)]
    
    pattern <- if (freq == "M") cbs_freq else paste0(cbs_freq, "0*") 
    replacement <- if (freq == "Y") "" else freq
    freq_data$Perioden <- sub(pattern, replacement, freq_data$Perioden)  
    
    return(freq_data)
  }
  
  freq_list <- sapply(frequencies, FUN = select_freq, simplify = FALSE)
  
  split_dimensions <- function(x) {
    if (length(dimensions) > 0) {
      return(split(x, x[ , dimensions, with = FALSE]))
    } else {
      return(list(x))
    }
  }
  
  # split data over the dinmensions
  freq_data_split <- lapply(freq_list, FUN = split_dimensions)
  
  
  check_ts <- function(tsname_info) {
    
    equal <- TRUE
  
    tsname <- tsname_info[1]
    topic <- tsname_info["Topic_Key"]
    
    if (length(dimensions) > 0) {
      dim_key <- paste(tsname_info[paste0(dimensions, "_Key")], collapse = ".")
    } else {
      dim_key <- 1
    }
    
    check_ts_freq <- function(freq) {
      
      ts_data_cbs <- freq_data_split[[freq]][[dim_key]][, topic, with = FALSE]
      ts_data_cbs <- suppressWarnings(as.numeric(ts_data_cbs[[1]]))
      
      ts_data_get_ts <- as.numeric(x[[freq]][ , tsname])
      if (!identical(ts_data_cbs, ts_data_get_ts)) {
        cat(sprintf("difference for timeseries %s and frequency %s\n", 
                    tsname, freq))
        cat("ts_data_cbs\n")
        print(ts_data_cbs)
        cat("ts_data_get_ts\n")
        print(ts_data_get_ts)
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    
    freq_errors <- sapply(frequencies, FUN = check_ts_freq)
    
    return(all(freq_errors))
    
  }
  
  # loop over the rows of the ts_names matrix
  ts_names_mat <- as.matrix(ts_names)
  ret <- apply(ts_names_mat, FUN = check_ts, MARGIN = 1)
  return(all(ret))
}
