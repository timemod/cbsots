# This function returns the filters used when downloading timeseries
# (including select). If the url for the filters is longer than MAX_URL_LEN,
# then the longest filter is dropped.
#' @importFrom utils URLencode
get_download_filters <- function(id, selected_code, cbs_code, frequencies, 
                                 min_year, period_keys, base_url,
                                 download_all_keys) {
  
  get_dimension_filter <-function(dimension) {
    if (nrow(cbs_code[[dimension]]) > nrow(selected_code[[dimension]])) {
      filter <- selected_code[[dimension]]$Key
    } else {
      filter <- NULL
    }
    return(filter)
  }
  
  if (!download_all_keys) {
    filters <- sapply(names(cbs_code), FUN = get_dimension_filter, 
                      simplify = FALSE)
    dimensions <- setdiff(names(cbs_code), "Topic")
    if (length(filters$Topic) > 0) {
      filters$select <- c("ID", dimensions, "Perioden", filters$Topic)
    }
    filters$Topic <- NULL
    # remove all NULL filters:
    filters <- filters[!sapply(filters, FUN = is.null)]   
  } else {
    filters <- list()
  }
  
  period_filter <- get_period_filter(period_keys,
    frequencies = frequencies,
    min_year = min_year
  )
  
  if (!is.null(period_filter)) {
    filters <- c(list(Perioden = period_filter), filters)
  }
  
  if (length(filters) == 0) {
    return(filters)
  }

  if (missing(base_url) || is.null(base_url)) {
    get_base_url <- utils::getFromNamespace("get_base_url", "cbsodataR")
    base_url <- get_base_url()
  }
  
  if (DEBUG) {
    cat("Filters before:\n")
    print(filters)
    cat("\n")
    cat("base_url:\n")
    print(base_url)
    cat("\n")
  }
  
 
  # Not check if the url for the chosen filters is not longer than MAX_URL_LEN.
  # Drop filters is this is necessary
 
  get_query <- utils::getFromNamespace("get_query", "cbsodataR")
  url_part1 <- sprintf("%s/ODataFeed/odata/%s/TypedDataSet?$format=json",
                       base_url, id)
  while (length(filters) > 0) {
    queries <- sapply(names(filters),
      FUN = function(x) do.call(get_query, filters[x]),
      simplify = FALSE
    )
    if (DEBUG) {
      cat("queries:\n")
      print(queries)
      cat("\n")
    }
    url <- paste0(url_part1, do.call(paste0, queries))
    url <- URLencode(url)
    if (nchar(url) <= MAX_URL_LEN) {
      break
    }
    query_lengths <- sapply(queries, FUN = nchar)
    longest_query <- names(which.max(query_lengths))
    if (longest_query == "Perioden" && !is.null(frequencies) && 
        !(is.null(min_year) || is.na(min_year))) {
      min_year <- NA
      filters$Perioden <- get_period_filter(period_keys,
        frequencies = frequencies,
        min_year = min_year
      )
      cat("\nmin_year", paste0("(", min_year, ")"), 
          "not used to filter on Periods while downloading because the length",
          "\nof the URL would be too long.\n\n")
    } else {
      filters <- within(filters, rm(list = longest_query))
      cat("\nDownload filter for dimension", longest_query, "dropped because",
          "the length of the URL would be\ntoo long.\n\n")
    }
  }
  
  cat("Filters:\n")
  print(filters)
  cat("\n")
  
  return(filters)
}

# This function returns a period filter for downloading CBS data
# ARGUMENTS:
#  * period_keys  a character vector with the period keys in the meta data
#  * frequencies  a character vector with frequencies ("Y", "H", "Q" or "M").
#  * min_year     a numeric specifying the minimum_year
get_period_filter <- function(period_keys, frequencies, min_year) {
  period_key_info <- parse_period_keys(period_keys)
  
  available_years <- unique(period_key_info$year)
  available_freqs <- unique(period_key_info$freq)
  
  filter_freqs <- !is.null(frequencies) 
  if (filter_freqs) {
    frequencies <- check_frequencies(frequencies, 
                                     period_key_info = period_key_info)
    filter_freqs <- !all(available_freqs %in% frequencies)
  }
  
  filter_years <- !is.null(min_year) && !is.na(min_year)
  
  if (filter_years) {
    check_min_year(min_year, period_key_info = period_key_info)
    filter_years <- min(available_years) <= min_year
  }
  
  # prevent warnings R CMD check
  freq <- NULL
  year <- NULL
  
  if (filter_years)  {
    if (filter_freqs) {
      # Both min_year and frequencies specified.
      period_keys <- subset(
        period_key_info,
        freq %in% frequencies & year >= min_year
      )$key
      if (length(period_keys) < nrow(period_key_info)) {
        period_filter <- period_keys
      } else {
        period_filter <- NULL
      }
    } else {
      # min_years used, but not frequencies
      years <- as.character(available_years[available_years >= min_year])
      period_filter <- cbsodataR::has_substring(years)
    }
  } else if (filter_freqs) {
    # only filter on frequencies
    freqs_cbs <- subset(freq_table, freq %in% frequencies)$freq_cbs
    period_filter <- cbsodataR::has_substring(freqs_cbs)
  } else {
    period_filter <- NULL
  }
  
  return(period_filter)
}