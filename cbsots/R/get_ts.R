# Functie cbs_table2ts downloadt CBS-tabellen, leest deze in en 
# converteert deze naar tijdreeksen volgens een bepaalde codering.
#
#
# INPUT
#   id              CBS open data table (inclusief extensie zoals NED).
#   naam_kort       Korte omschrijving van de tabel, deze bepaalt de 
#                   naam van de tijdreekscodefile en de outputfiles.
#   download        Een logical die aangeeft of de CBS-data opnieuw gedownload
#                   moet worden. Als deze FALSE is, dan worden
#                   de gegevens ingelezen die eerder gedownload zijn 
#                   en opgeslagen zijn in de map "ruwe_cbs_data".               
#   tijdreekscode_dir Naam van de map waarin de tijdreekscodebestanden staan
#                   (standaard is dit "tijdreekscodes").
#   raw_cbs_dir    Naam van de map waarin de gedownloade CBS-tabellen worden
#                   geschreven (standaard is dit "ruwe_cbs_data").
#
# RETURN:  een lijst met de volgende elementen:
#     ts_namen   Een data.table met ts namen en de bijbehorende 
#                 CBS-onderwerpen en dimensies (handig voor controle).
#     M           Maandreeksen (indien aanwezig)
#     Q           Kwartaalreeksen (indien aanwezig)
#     Y           Jaarreeksen (indien aanwezig)
#

#' Return time indices
#' @param id table id
#' @param table_code_collection  a \code{table_code_collection} object
#' @param download If \code{TRUE}, data are downloaded, otherwise the data
#' is read from the previously downloaded data in directory \code{raw_cbs_dir}
#' @param raw_cbs_dir directory where the raw downloaded data are stored
#' @importFrom regts as.regts
#' @importFrom regts update_ts_labels
#' @importFrom stats as.formula
#' @importFrom cbsodataR get_data
#' @export
get_ts <- function(id, table_code_collection, download,  
                   raw_cbs_dir = "raw_cbs_data") {

  if (!inherits(table_code_collection, "table_code_collection")) {
    stop("Argument table_code_collection is not a table_code_collection object")
  }
  
  table_ids <- names(table_code_collection$table_code)
  table_ids_lower <- tolower(table_ids)
  id_lower <- tolower(id)
  
  if (!id_lower %in% table_ids_lower) {
    stop(paste("Table", id, "not in table_code_collection"))
  }
  
  idx <- match(id_lower, table_ids_lower)
  id <- table_ids[idx]
  
  table_code <- table_code_collection$table_code[[id]]
  cbs_code <- get_cbs_code(id, cache = TRUE)
  
  convert_code <- function(groep) {
  
    code <- table_code$codes[[groep]]
    code <- code[Select == TRUE,]
    
    # check for duplicates in Code
    if (anyDuplicated(code$Code)) {
      duplicates <- unique(code$Code[duplicated(code$Code)])
      stop(paste0("Duplicate codes found for ",  groep, ":\n", 
                  paste(duplicates, collapse = "\n")), "\n.")
    }
    
    unknown_keys <- setdiff(code$Key, cbs_code[[groep]]$Key)
    if (length(unknown_keys) > 0) {
      stop(paste0("Unknown keys in ts code for ", groep, ":\n", 
                  paste(unknown_keys, collapse = "\n")), "\n.")
    }
    
    
    # Als de ts-code niet opgegeven is, dan wordt er voor deze dimensie
    # geen suffix worden toegevoegd aan de tijdreeksnaam en de label.
    code[(is.na(Code) | trimws(Code) == ""), c("Title", "Code") := ""]
    
    #cat("code = \n")
    #print(code)
    return(code)
  }
  
  # Convert de codes van het CBS en de tijdreeksen. Let op: gebruik
  # de volgorde van de codering zoals in ts_code, die is gebaseerd
  # op de volgorde van de sheets in de tijdreekscodefile.
  # De volgorde van de sheets bepaalt de naamgeving:
  # naam = (ts-code sheet 1) + (ts-code sheet 2) ...
  code <- sapply(table_code$order, FUN = convert_code, simplify = FALSE)

  # downloaden of inlezen van de echte data
  na_strings <- c("       .", ".", "       -")   
  # dit zijn de teksten die het CBS gebruikt voor NA-waarden
  
  dimensies <- setdiff(names(table_code$codes), "Topic")
  
  if (download) {
    
    cat(paste("Downloading table", id, "...\n"))

    # maak nu voor elke dimensie een lijst met keys waarop gefilterd moet worden,
    # of NULL als er niet gefilterd hoeft te worden worden
    maak_filter <-function(dimensie) {
      if (nrow(cbs_code[[dimensie]]) > nrow(code[[dimensie]])) {
        filter <- code[[dimensie]]$Key
      } else {
        filter <- NULL
      }
      return(filter)
    }
   
    filters <- sapply(dimensies, FUN = maak_filter, simplify = FALSE)
    
    if (length(filters) > 0) {
      filters <- filters[sapply(filters, FUN = function(x) {!is.null(x)})]   
      cat("Filters gebruikt bij het downloaden:\n")
      print(filters)
    }
    
    argumenten <- c(list(id = id, recode = FALSE, 
                         dir = file.path(raw_cbs_dir, id)), filters)
    data <- do.call(get_data, argumenten)

    # vervang na_string door een lege string, en zet resultaat om naar een
    # data.table
    data <- as.data.table(lapply(data, 
                FUN = function(x) {ifelse(x %in% na_strings, "", x)}))
    
    data[, ID := NULL]
    
    cat("Done\n")
    
  } else {
    # inlezen van eerder gedownloade file
    data_file <- file.path(raw_cbs_dir, id, "data.csv")
    if (!file.exists(data_file)) {
      stop(paste("Download eerst tabel", id))
    }
    cat(paste("Reading table", id, "from", data_file, "...\n"))
    data <- fread(data_file, drop = "ID", na.strings = na_strings)
    cat("Done\n")
  }

  # verwijder de topics die niet voorkomen in de tijdreekscodering
  ongebruikte_topics <- setdiff(cbs_code$Topic$Key, code$Topic$Key)
  if (length(ongebruikte_topics) > 0) {
    data[, (ongebruikte_topics) := NULL]
  }
  
  # Vervang de topic-namen met de overeenkomende tijdreekscodes
  setnames(data, old = code$Topic$Key, new = code$Topic$Code)
  
  # vervang de dimensienamen
  for (dimensie in dimensies) {
    
    keys <- code[[dimensie]]$Key
    ts <- code[[dimensie]]$Code

    # selecteer de rijen waarvoor de dimensie voorkomt in de ts-codering,
    data <- data[eval(as.name(dimensie)) %in% keys]
    
    # Hernoem de dimensie-kolommen door de overeenkomende tijdreekscodes
    dim_index <- match(data[[dimensie]], keys)
    data[[dimensie]] <- ts[dim_index]
  }
  
  if (!"Perioden" %in% colnames(data)) {
    stop(paste("Table", id, "does not contain timeseries"))
  }

  # In data frame data zijn de tijdreeksen voor hetzelfde onderwerp maar
  # met verschillende dimensies gestapeld in een enkel kolom.
  # Daarom gebruiken we functie dcast (to "cast" betekent "gieten")
  # om het data frame in de juiste vorm te gieten, namelijk voor elke
  # tijdreeks een aparte kolom
  if (length(dimensies) > 0) {
    
    melted <- melt(data, id.vars = c("Perioden", dimensies),
                   measure.vars = code$Topic$Code, variable.name = "Topic")
    
    formula <- as.formula(paste("Perioden ~", 
                          paste(names(code), collapse = " + ")))
    data <- dcast(melted, formula = formula, sep = "") 
  }

  ts_namen_en_labels <- maak_ts_namen_en_labels(code)

  ts_ts <- maak_tijdreeksen(data, ts_namen_en_labels$labels)
  
  return(structure(c(list(ts_namen = ts_namen_en_labels$ts_namen), ts_ts),
         class = "table_ts"))
}

maak_ts_namen_en_labels <- function(code) {
  # Bereken de ts-namen en de bijbehorende labels door het uitproduct
  # van alle keys (topic-keys en dimensie-keys) te berekenen.
  # Hiervoor wordt functie CJ van het data.table pakket gebruikt.
  
  keys <- lapply(code, FUN = function(x) {x$Key})
  keys <- do.call(CJ, c(keys, sorted = FALSE))

  codes <- lapply(code, FUN = function(x) {x$Code})
  codes <- do.call(CJ, c(codes, sorted = FALSE))
  namen <- do.call(paste0, codes)

  main_labels <- list(code[[1]]$Title)  
  extra_labels <- lapply(code[-1], 
     FUN = function(x) {ifelse(x$Title == "", "", paste0("(", x$Title, ")"))})
  labels <- c(main_labels, extra_labels)
  labels <- do.call(CJ, c(labels, sorted = FALSE))
  labels <- do.call(paste, labels)
  names(labels) <- namen

  ts_namen <- cbind(naam = namen, keys, labels = labels)
 
  # sorteer ts namen
  ts_namen <- ts_namen[order(namen), ]

  return(list(ts_namen = ts_namen, labels = labels))
}

maak_tijdreeksen <- function(data, labels) {

  # conversietabel cbs-frequenties <-> regts-frequenties
  freq_table <- c(JJ = "Y", KW = "Q", MM = "M")
 
  cbs_frequenties <- unique(sub("\\d+(.+?)\\d+", "\\1", data$Perioden))
  missing <- setdiff(cbs_frequenties, names(freq_table))
  if (length(missing) > 0) {
    stop(paste("Onbekende frequenties", paste(missing, collapse = " "), 
               "gevonden"))
  }
  frequenties <- freq_table[cbs_frequenties]

  maak_tijdreeksen_freq <- function(i) {
    # maak tijdreeksen aan voor de frequentie met index i
  
    # verzamel data
    freq <- frequenties[i]
    cbs_freq <- cbs_frequenties[i]
    data_freq <- data[grepl(cbs_freq, Perioden)]
    pattern <- paste0(cbs_freq, "0*") # voeg 0* toe vanwege JJ00 in cbs-data
    data_freq$Perioden <- sub(pattern, freq, data_freq$Perioden)
    
    # converteer data frame naar tijdreeks
    ts_freq <- as.regts(data_freq, time_column = "Perioden") 
  
    # voeg labels to
    ts_freq  <- update_ts_labels(ts_freq, labels)
  
    # sorteer kolommen alfabetisch
    ts_freq <- ts_freq[, order(colnames(ts_freq)), drop = FALSE]
  
    return(ts_freq)
  }
  
  reeksen <- lapply(seq_along(frequenties), FUN = maak_tijdreeksen_freq)
  names(reeksen) <- frequenties
  
  return(reeksen)
}