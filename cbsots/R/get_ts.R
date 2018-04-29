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
#' correspondings files in directory \code{raw_cbs_dir} are missing.
#' The default is \code{FALSE}
#' @param download This argument is deprecated and has been replaced by argument
#' \code{refresh}.
#' @param include_meta include meta data
#' @param min_year  the minimum year of the returned timeseries. Data 
#' for years before \code{min_year} are disregarded. Specify \code{NULL}
#' or \code{NA} to not impose a minimum year
#' @param frequencies a character specifying the frequencies of the 
#' required timeseries. For example, specify \code{"Q"} for quarterly series only,
#' \code{"YQ"} for both yearly and quarterly series, and \code{"M"} for 
#' monthly series only. 
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
#' #' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#'  \item{Y}{Yearly timeseries (if present)}
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
  
  if (!is.null(frequencies)) {
    if (!is.character(frequencies) || length(frequencies) > 1) {
      stop("Argument frequencies should be a character of length 1 (e.g. \"yq\")")
    }
    frequencies <- toupper(unique(unlist(strsplit(frequencies, ""))))
    freqs_error <- ! frequencies %in% c("Y", "Q", "M")
    if (any(freqs_error)) {
      stop(paste("Unknown frequencies", 
                 paste(frequencies[freqs_error], collapse = " "),
                "specified"))
    }
  }
  
  if (!missing(download)) {
    warning(paste("Argument download is deprecated and has been replaced by",
                  "argument refresh"))
    if (missing(refresh)) {
      refresh <- download
    } else {
      warning("Deprecated argument download is overruled by argument refresh")
    }
  }
  
  ts_code <- convert_ts_code(ts_code)
  
  if (!inherits(ts_code, "ts_code")) {
    stop("Argument ts_code is not a ts_code object")
  }
  
  table_ids <- names(ts_code$table_code)
  table_ids_lower <- tolower(table_ids)
  id_lower <- tolower(id)
  
  if (!id_lower %in% table_ids_lower) {
    stop(paste("Table", id, "not in ts_code"))
  }
  
  idx <- match(id_lower, table_ids_lower)
  id <- table_ids[idx]
  
  table_code <- ts_code$table_code[[id]]
  
  data_dir <- file.path(raw_cbs_dir, id)
  
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  `.` <- NULL; Select <- NULL; Code <- NULL; Perioden <- NULL
  
  convert_code <- function(name) {
  
    code <- table_code$codes[[name]]
    
    code <- code[Select == TRUE,]
    
    # If the code is empty, then also make the title empty.
    code[(is.na(Code) | trimws(Code) == ""), c("Title", "Code") := ""]
    
    return(code)
  }
  
  # Convert codes and create a list with the order given by table_code$order.
  # This order determines how the timeseries names are created.
  code <- sapply(table_code$order, FUN = convert_code, simplify = FALSE)

  # strings used by the CBS to represent NA values
  na_strings <- c("       .", ".", "       -")   
  
  dimensions <- setdiff(names(table_code$codes), "Topic")
  
  if (!refresh) {
    read_ok <- FALSE
    meta <- read_meta_data(data_dir)
    if (!is.null(meta)) {
      check_language(meta)
      cbs_code <- get_cbs_code(meta)
      code <- check_code(code, cbs_code)
      data <- read_data(data_dir, na_strings = na_strings)
      if (!is.null(data)) {
        period_keys <- get_period_keys(meta, min_year, frequencies)
        read_ok <- check_read_data(data, code, period_keys = period_keys)
        if (read_ok && (!is.null(min_year) || !is.null(frequencies))) {
          data <- data[Perioden %in% period_keys]
        }
      }
    }
  }
  
  if (refresh || !read_ok) {
    ret <- download_table(id, raw_cbs_dir = raw_cbs_dir, code = code, 
                          min_year = min_year, frequencies = frequencies,
                          na_strings = na_strings, base_url = base_url, 
                          download_all_keys = download_all_keys)
    meta <- ret$meta
    data <- ret$data
    cbs_code <- ret$cbs_code
  }
  

  select_code_rows <- function(name) {
    # remove all rows with an empty Code, except if the code_table has 1 row
    table <- code[[name]]
    if (nrow(table) > 1) {
      table <- table[nchar(Code) > 0, ]
      if (nrow(table) == 0) {
        stop(paste("No single Code specified for", name))
      }
    }
    return(table)
  }
  
  # Convert the code tables based on the code column
  code <- sapply(table_code$order, FUN = select_code_rows, simplify = FALSE)
  
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

  ts_ts <- maak_tijdreeksen(data, ts_names_en_labels$labels)

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
  
    # prevent notes from R CMD check about no visible binding for global
    Perioden <- NULL
    
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
