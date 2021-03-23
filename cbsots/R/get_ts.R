#' Return CBS timeseries
#' 
#' @param id table id
#' @param ts_code a \code{ts_code} object. This object can be created
#' and modified with function \code{\link{edit_ts_code}}, which starts a Shiny
#' app.
#' @param raw_cbs_dir directory where the raw downloaded data are stored.
#' @param refresh should the data in directory \code{raw_cbs_dir} be refreshed?
#' If \code{TRUE}, the data are always downloaded from the 
#' CBS website. Otherwise  the data will only be downloaded if the 
#' correspondings files in directory \code{raw_cbs_dir} are missing or not 
#' complete (missing dimension keys). The default is \code{FALSE}.
#' Note that data may also be downloaded when new keys are selected 
#' in the timeseries coding.
#' @param download This argument overrules argument \code{refresh}. If \code{FALSE},
#' then data all never downloaded again. You will get an error if the files in
#' directory \code{raw_cbs_dir} are missing or not 
#' complete (missing dimension keys). If \code{TRUE} then data are always 
#' downloaded.
#' @param include_meta include meta data
#' @param min_year  the minimum year of the returned timeseries. Data 
#' for years before \code{min_year} are disregarded. Specify \code{NULL}
#' or \code{NA} to not impose a minimum year
#' @param frequencies a character string specifying the frequencies of the 
#' returned timeseries. Specify `"Y"`, `"H"`, `"Q"` or `"M"` for annual, 
#' semi-annual, quarterly or monthly series, respectively. It is possible to specify a 
#' combination of these characters, e.g. `"YQ"` for annual and quarterly series.
#' Another example: to retrieve annual, quarterly and monthly series simultaneously,
#' specify `"YQM"`. The function returns a list with a component for each 
#' specified frequency.
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @param download_all_keys This option specifies how to download data. By default,
#' for each table dimension (excluding the topic) only the selected keys in the 
#' timeseries coding are downloaded.  Although this can significantly reduce 
#' downloading  time, this approach has the disadvantage that it is necessary to 
#' download the data again when a new dimension key is selected in the 
#' timeseries coding. To prevent that, use argument \code{download_all_keys = TRUE},
#' then all keys are downloaded for each dimension. 
#' @return a list with class \code{table_ts}, with the following components
#'  \item{Y}{Annual timeseries (if present)}
#'  \item{H}{Semi-annual timeseries (if present)}
#'  \item{Q}{Quarterly timeseries (if present)}
#'  \item{M}{Monthly timeseries (if present)}
#'  \item{ts_names}{A data frame with an overview of the timeseries names}
#'  \item{meta}{Meta data, only if argument \code{include_meta} is \code{TRUE}}
#'
#' @importFrom regts as.regts
#' @importFrom regts update_ts_labels
#' @importFrom stats as.formula
#' @importFrom utils modifyList
#' @export
get_ts <- function(id, ts_code, refresh = FALSE, raw_cbs_dir = "raw_cbs_data",
                   include_meta = FALSE, min_year = NULL, frequencies = NULL,
                   download, base_url = NULL, download_all_keys = FALSE) {

  if (!is.null(min_year) && is.na(min_year)) {
    min_year <- NULL
  }
  if (!is.null(frequencies) && is.na(frequencies)) {
    frequencies <- NULL
  }
  
  if (!is.null(frequencies)) {
    if (!is.character(frequencies) || length(frequencies) > 1) {
      stop("Argument frequencies should be a character of length 1 (e.g. \"yq\")")
    }
    frequencies <- toupper(unique(unlist(strsplit(frequencies, ""))))
    freqs_error <- ! frequencies %in% c("Y", "H", "Q", "M")
    if (any(freqs_error)) {
      stop(paste("Unknown frequencies", 
                 paste(frequencies[freqs_error], collapse = " "),
                "specified"))
    }
  }
  
  if (!missing(download)) {
    refresh <- download
  }
  
  ts_code <- convert_ts_code(ts_code)
  
  if (!inherits(ts_code, "ts_code")) {
    stop("Argument ts_code is not a ts_code object")
  }
  
  table_ids <- names(ts_code)
  table_ids_lower <- tolower(table_ids)
  id_lower <- tolower(id)
  
  if (!id_lower %in% table_ids_lower) {
    stop(paste("Table", id, "not in ts_code"))
  }
  
  idx <- match(id_lower, table_ids_lower)
  id <- table_ids[idx]
  
  table <- ts_code[[id]]
  dimensions <- setdiff(names(table$codes), "Topic")
  
  data_dir <- file.path(raw_cbs_dir, id)
  
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  `.` <- NULL; Select <- NULL; Code <- NULL; Perioden <- NULL
  
  select_code <- function(name) {
    code <- table$codes[[name]]
    return(code[Select == TRUE, ])
  }
  
  # Select all rows with Select = TRUE. The ordering of the 
  # dimensionswill be equal to table$order, this ordering is important
  # because it determines how the timeseries names are created.
  selected_code <- sapply(table$order, FUN = select_code, simplify = FALSE)
  
  
  if (!refresh) {
    read_result <- read_table(id, data_dir, code = table$codes, 
                              selected_code = selected_code, 
                              min_year = min_year, frequencies = frequencies)
    if (is.null(read_result) && !missing(download) && !download) {
      stop(paste("The files in directory", file.path(raw_cbs_dir, id), 
                 "are incomplete or corrupt. Please download the data again."))
    }
  }
  
  if (refresh || is.null(read_result)) {
    download_table(id, raw_cbs_dir = raw_cbs_dir, code = table$codes, 
                   selected_code = selected_code, min_year = min_year, 
                   frequencies = frequencies, base_url = base_url, 
                   download_all_keys = download_all_keys)
    read_result <- read_table(id, data_dir, code = table$codes, 
                              selected_code = selected_code, 
                              min_year = min_year, frequencies = frequencies,
                              read_downloaded_data = TRUE)
    if (is.null(read_result)) {
      stop("Error reading the downloaded data")
    }
  }
  
  meta <- read_result$meta
  data <- read_result$data
  cbs_code <- read_result$cbs_code
  
  select_code_rows <- function(name) {
    # remove all rows with an empty Code, except if the select_code table has 1 row
    table <- selected_code[[name]]
    if (nrow(table) > 1) {
      table <- table[nchar(Code) > 0, ]
      if (nrow(table) == 0) {
        stop(paste("No single Code specified for", name))
      }
    } else {
      # If the code is empty, then also make the title empty.
      table[(is.na(Code) | trimws(Code) == ""), c("Title", "Code") := ""]
    }
    return(table)
  }
  
  # Convert the code tables based on the code column. 
  code <- sapply(names(selected_code), FUN = select_code_rows, simplify = FALSE)
  
  # remove  topics that are not present in code
  unused_topics <- setdiff(cbs_code$Topic$Key, code$Topic$Key)
  if (length(unused_topics) > 0) {
    data[, (unused_topics) := NULL]
  }

  # Rename the topic names
  setnames(data, old = code$Topic$Key, new = code$Topic$Code)

  # rename the dimension keys with the code
  for (dimension in dimensions) {
    
    keys  <- code[[dimension]]$Key
    codes <- code[[dimension]]$Code

    # select rows
    data <- data[get(dimension) %in% keys]
    
    # rename dimensions
    dim_index <- match(data[[dimension]], keys)
    data[[dimension]] <- codes[dim_index]
  }
  
  if (!"Perioden" %in% colnames(data)) {
    stop(paste("Table", id, "does not contain timeseries"))
  }
  

  # In data frame data zijn de tijdreeksen voor hetzelfde onderwerp maar
  # met verschillende dimensions gestapeld in een enkel kolom.
  # Daarom gebruiken we functie dcast (to "cast" betekent "gieten")
  # om het data frame in de juiste vorm te gieten, namelijk voor elke
  # tijdreeks een aparte kolom
  if (length(dimensions) > 0) {
    
    melted <- melt(data, id.vars = c("Perioden", dimensions),
                   measure.vars = code$Topic$Code, variable.name = "Topic")
    
    formula <- as.formula(paste("Perioden ~", 
                          paste(names(code), collapse = " + ")))
    
    data <- dcast(melted, formula = formula, sep = "") 
  }

  ts_names_en_labels <- maak_ts_names_en_labels(code)

  ts_ts <- create_timeseries(data, ts_names_en_labels$labels)

  ret <- c(ts_ts, list(ts_names = ts_names_en_labels$ts_names))
  
  if (include_meta) {
    ret$meta <- clean_meta_data(meta, code, cbs_code, dimensions)
  }
           
  return(structure(ret, class = "table_ts"))
}

maak_ts_names_en_labels <- function(code) {
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

  ts_names <- cbind(name = namen, keys, labels = labels)
 
  # sorteer ts namen
  ts_names <- as.data.frame(ts_names[order(namen), ])

  return(list(ts_names = ts_names, labels = labels))
}

create_timeseries <- function(data, labels) {

  # conversion table CBS-frequencies <-> cbsots-frequencies
  freq_name_table <-   c(JJ = "Y", HJ = "H", KW = "Q", MM = "M")
  freq_number_table <- c(JJ = 1,   HJ = 2,   KW = 4,   MM = 12 )
  
  frequencies_cbs <- unique(sub("\\d+(.+?)\\d+", "\\1", data$Perioden))
  missing <- setdiff(frequencies_cbs, names(freq_name_table))
  if (length(missing) > 0) {
    stop(paste("Unknown frequencies", paste(missing, collapse = " "), 
               "in CBS data"))
  }
  freq_names   <- freq_name_table[frequencies_cbs]
  freq_numbers <- freq_number_table[frequencies_cbs]
 
  # Create a timeseries for a specific frequency. 
  # cbs_freq is the frequency as specific in the CBS data, e.g. JJ 
  # (yearly series) or KW (quarterly series).
  # freq_number is the frequency as a number (e.g. 4 for quartely series)
  create_timeseries_freq <- function(freq_cbs, freq_number) {
   
    # prevent notes from R CMD check about no visible binding for global
    Perioden <- NULL
    
    data_freq <- data[grepl(freq_cbs, Perioden)]
    
    # Replace CBS frequency indicators, including trailing zeros,
    # witg generic regts frequency separator ("-"), or nothing (yearly series)
    # Note: for yearly series, the indicator is sometimes JJ00 
    # (e.g. 20020JJ00).
    pattern <- paste0(freq_cbs, "0*") 
    replacement <- if (freq_number == 1) "" else "_"
    data_freq$Perioden <- sub(pattern, replacement, data_freq$Perioden)
    
    # convert data to timeseries
    ts_freq <- as.regts(data_freq, time_column = "Perioden", 
                        frequency = freq_number) 
  
    # add labels
    ts_freq  <- update_ts_labels(ts_freq, labels)
  
    # sort columns alphabetically
    ts_freq <- ts_freq[, order(colnames(ts_freq)), drop = FALSE]
  
    return(ts_freq)
  }
  
  tseries <- mapply(create_timeseries_freq, frequencies_cbs, freq_numbers,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(tseries) <- freq_names
  
  return(tseries)
}

# Remove entries in the meta data that are not used to create the tables.
clean_meta_data <- function(meta_data, code, cbs_code, dimensions) {
  
  convert_meta <- function(name) {
    sel <- match(code[[name]]$Key, meta_data[[name]]$Key)
    return(meta_data[[name]][sel,  , drop = FALSE])
  }
  dimension_meta <- sapply(dimensions, FUN = convert_meta, simplify = FALSE)
  for (name in names(dimension_meta)) {
    meta_data[[name]] <- dimension_meta[[name]]
  }

  # now convert DataProperies. First remove all Topics that are not used.
  dp <- as.data.table(meta_data$DataProperties)
  
  remove_keys <- setdiff(cbs_code$Topic$Key, code$Topic$Key)
  dp <- dp[!dp$Key %in% remove_keys, ]
  
  # Next, remove all TopicGroups that have no children Topics any more.
  # First assume that all TopicGroups are not used, then move backwards trough
  # the data properties and register all used TopicGroups.
  used <- dp$Type != "TopicGroup"
  for (i in nrow(dp):1) {
    parent_id <- dp$ParentID[i]
    if (is.na(parent_id)) {
      next
    }
    type <- dp$Type[i]
    if (type == "Topic") {
      used[pmatch(parent_id, dp$ID)] <- TRUE 
    } else if (used[i] && type == "TopicGroup") {
      used[pmatch(parent_id, dp$ID)] <- TRUE 
    }
  }
  
  meta_data$DataProperties <- dp[used, drop = FALSE]
  return(meta_data)
}
