#' Read timeseries coding from an Excel file.
#' 
#' The format of the Excel file is the old-style timeseries coding.
#' 
#' @param xlsx_files a named character vector with the names of one or more Excel
#' files with timeseries information. The names are the table ids as
#' defined by the cbs.
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @export
read_ts_code_xlsx <- function(xlsx_files) {
  
  table_ids <- names(xlsx_files)
  table_ids_lower <- tolower(table_ids)
  table_ids_cbs <- as.character(get_table_list(select = "Identifier")[, 1])
  
  # case insensitive
  table_ids_cbs_lower <- tolower(table_ids_cbs)
  
  unknown_ids <- setdiff(table_ids_lower, table_ids_cbs_lower)
  if (length(unknown_ids) > 0) {
    # TODO: better error message
    print(unknown_ids)
    stop("Unknown IDS found")
  }
 
  idx <- match(table_ids_lower, table_ids_cbs_lower)
  table_ids <- table_ids_cbs[idx]

  make_table <- function(i) {
    table_id <- table_ids[i]
    xlsx_file <- xlsx_files[i]
    cat(paste("Reading table", table_id, "from", xlsx_file, "...\n"))
    return(read_ts_table_xlsx(table_id = table_id, xlsx_file = xlsx_file))
  }
  table_code <- lapply(seq_along(table_ids), FUN = make_table) 
  names(table_code) <- table_ids
  
  # order tables alphabetically
  table_code <- table_code[order(names(table_code))]
  
  return(structure(list(package_version = packageVersion("cbsots"),
                        table_code = table_code),
         class = "table_code_collection"))
}

# internal function
read_ts_table_xlsx <- function(table_id, xlsx_file) {
  
  table <- create_new_table(table_id)
  dimensions <- setdiff(names(table$codes), "Topic")
  ts_code <- lees_tijdreekscodes(tijdreekscodefile = xlsx_file, 
                                 dimensies = dimensions)
  
  merge_code <- function(group) {
    # Merge de opendata- en tijdreekscode
    
    code <- table$codes[[group]]
    
    ts  <- ts_code[[group]]
    
    ts <- fix_ts(ts, code, group)
    
    code$Code[match(ts$Key, code$Key)] <- ts$Code
  
    code$Select <- code$Key %in% ts$Key
    
    code <- code[, c("Key", "Select", "Code", "Title", "OrigKeyOrder")]
  
    code <- order_code_rows(code, code$OrigKeyOrder)
    
    return(code)
  }
  
  # Merge de codes van het CBS en de tijdreeksen. Let op: gebruik
  # de volgorde van de codering zoals in ts_code, die is gebaseerd
  # op de volgorde van de sheets in de tijdreekscodefile.
  # De volgorde van de sheets bepaalt de naamgeving:
  # naam = (ts-code sheet 1) + (ts-code sheet 2) ...
  codes <- sapply(names(table$codes), FUN = merge_code, simplify = FALSE)
 
  table$codes <- codes
  table$order <- names(ts_code)
  table$last_modified <- Sys.time()
  return(table)
}

# Functie lees_tijdreekscodes is een hulpfunctie voor functie cbs_table2ts.
# De functie leest de tijdreekscodes in van de tijdreekscodefile.

# INPUT
#   tijdreekscodefile de naam van de tijdreekscode-file
#   dimensies       een character vector met de namen van de dimensies,
#                   zoals ingelezen van de CBS-data.
#
# RETURN:  Een lijst met diverse data.tables die de ingelezen tijdreekscodering
#          bevatten.

lees_tijdreekscodes <- function(tijdreekscodefile, dimensies) {
  
  # Lees de sheetnamen van de tijdreekscodefile
  sheetnamen <- excel_sheets(tijdreekscodefile)
  
  groepen <- c("Topic", dimensies)
  
  # Controleer of er voor elke groep een overeenkomende sheet in de 
  # tijdreekscode voorkomt. De sheetnaam moet gelijk zijn aan de naam van de 
  # dimensie, behalve als de lengte van de groepnaam groter is dan 31 
  # (de maximale lengte van een sheetnaam in Excel): in dat geval moet de 
  # sheetnaam gelijk aan de eerste 31 karakters van de groepnaam.
  groepen_max_31 <- substr(groepen, 1, 31)
  if (anyDuplicated(groepen_max_31)) {
    stop(paste("ALARM!!! De eerste 31 karakters van de CBS-dimensies",
               "zijn niet uniek!"))
  }
  ontbrekende_sheets <- setdiff(groepen_max_31, sheetnamen)
  if (length(ontbrekende_sheets) > 0) {
    stop(paste0("De volgende sheets ontbreken in ", tijdreekscodefile, ":\n", 
                paste(ontbrekende_sheets, collapse = "\n")), "\n.")
  }
  
  # maak een lijst van codegroepen in de volgorde van de sheetnamen 
  # in de tijdreekscodefile
  volgorde <- match(sheetnamen, groepen_max_31)
  volgorde <- volgorde[!is.na(volgorde)]
  groepen <- groepen[volgorde]
  
  # Lees de ts stammen. Vervang NA-waarden of lege teksten in kolom ts met 
  # # "x" plus het nummer van de cbs-key.
  # topics <- as.data.table(read_excel(tijdreekscodefile, sheet = "Topic"))
  # topics <- topics[,  .(Key, Code)]
  #
  check_duplicaten <- function(df, naam, sheet) {
    # functie die controleert of er duplicaten in kolom naam van data frame df
    # staan, en zonodig een foutmelding geeft.
    x <- df[[naam]]
    if (anyDuplicated(x)) {
      duplicaten <- unique(x[duplicated(x)])
      stop(paste0("Duplicaten in ",  naam, " gevonden in sheet \"",
                  sheet, "\" van ", tijdreekscodefile, ":\n", 
                  paste(duplicaten, collapse = "\n")), "\n.")
    }
    return(invisible(NULL))
  }
  
  # lees nu de codesheets
  lees_codesheet <- function(groep) {
    
    # maximale lengte Excel-sheet is 31
    sheetnaam <- substr(groep, 1, 31)
    code <- as.data.table(read_excel(tijdreekscodefile, sheet = sheetnaam, 
                                     col_types = "text"))

    code <- code[, .(Key, Code)]

    if (groep == "Topic") {
      # voor het onderwerp (Topic) mag de tijdreekscode weggelaten worden:
      # in dat geval wordt de code automatisch gegenereerd op basis van het
      # nummer van de topic volgens de cbs-codering
      default_code <- gsub(".*?(\\d+)$", "x\\1", code$Key)
      code[, Code := ifelse(is.na(Code) | trimws(Code) == "",  default_code, 
                            Code)]
    }
    
    check_duplicaten(code, "Key", sheetnaam)
    check_duplicaten(code, "Code", sheetnaam)
    
    return(code)
  }
  
  code <- sapply(groepen, FUN = lees_codesheet, simplify = FALSE)
  
  return(code)
}     