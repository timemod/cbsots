download_table <- function(id, raw_cbs_dir, code, selected_code, min_year, 
                           frequencies, base_url, download_all_keys) {
  
  cat(paste("Downloading table", id, "...\n"))
  
  # first download the meta data, which is needed to create the filters
  # for downloading the table data
  meta <- download_meta(id, base_url = base_url)

  cbs_code <- get_cbs_code(meta)

  check_unknown_keys(id, selected_code, cbs_code)
  check_code(id, code, selected_code, cbs_code, downloaded = TRUE)
 
  # create a filter for each dimension if necessary
  get_dimension_filter <-function(dimension) {
    if (nrow(cbs_code[[dimension]]) > nrow(selected_code[[dimension]])) {
      filter <- selected_code[[dimension]]$Key
    } else {
      filter <- NULL
    }
    return(filter)
  }

  if (!download_all_keys) {
    dimensions <- setdiff(names(code), "Topic")
    dimension_filters <- sapply(dimensions, FUN = get_dimension_filter, simplify = FALSE)
    topic_filter <- get_dimension_filter("Topic")
    if (!is.null(topic_filter)) {
      select_filter <- list(select = c("ID", dimensions, "Perioden", 
                                       topic_filter))
    } else {
      select_filter <- NULL
    }
  } else {
    filters <- list()
    select_filter <- NULL
  }
  
  period_filter <- get_period_filter(meta$Perioden$Key, 
                                     frequencies = frequencies, 
                                     min_year = min_year)
  
  dimemsion_filters <- dimension_filters[!sapply(dimension_filters, 
                                                 FUN = is.null)]
  
  filters <- c(period_filter, dimension_filters, select_filter)
  
  # remove null filters
  if (length(filters) > 0) {
    filters <- filters[sapply(filters, FUN = function(x) {!is.null(x)})]   
    # Do not print filters, only the filters that we will actually use:
    #cat("Filters:\n")
    #print(filters)
  }
  
  # Now check
  cat("filters\n")
  print(filters)
  cat("\n")
  
  if (missing(base_url) || is.null(base_url)) {
    get_base_url <- utils::getFromNamespace("get_base_url", "cbsodataR")
    base_url <- get_base_url()
  }
  
  cat("base_url:\n")
  print(base_url)
  
  get_query <- utils::getFromNamespace("get_query", "cbsodataR")
  
  url_part1 <- sprintf("%s/ODataFeed/odata/%s/TypedDataSet?$format=json",
                 base_url, id)
  
  # it turns out that the maximum length of an URL is about 8000
  # drop the longest filter
  MAX_URL_LEN <- 8000
  while (TRUE) {
    filter_expressions <- sapply(names(filters),
                                 FUN = function(x) do.call(get_query, filters[x]),
                                 simplify = FALSE)
    
    cat("filter expressions\n")
    print(filter_expressions)
    cat("\n")
    url <- paste0(url_part1, do.call(paste0, filter_expressions))
    url <- URLencode(url)
    print(nchar(url))
    if (nchar(url) <= MAX_URL_LEN) {
       break
     }
     filter_expr_lengths <- sapply(filter_expressions, FUN = nchar)
     longest_filter <- names(filter_expressions)[which.max(filter_expr_lengths)]
     if (longest_filter == "Perioden" && !is.null(frequencies) && 
         !(is.null(min_year) || is.na(min_year))) {
       min_year <- NA
       filters$Perioden <- 
         get_period_filter(meta$Perioden$Key, 
                           frequencies = frequencies, 
                           min_year = min_year)$Perioden
    } else {
       filters <- within(filters, rm(longest_filter))
     }
  }
  
  arguments <- c(list(id = id, dir = file.path(raw_cbs_dir, id)), filters,
                 list(cache = TRUE, typed = TRUE))

  
  if (!is.null(base_url)) {
    arguments <- c(arguments, list(base_url = base_url))
  }
  
  data <- do.call(cbsodataR::cbs_download_table, arguments)
 
  return(invisible(NULL))
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
      # WARNING: this approach may result in a url that is too long.
      period_keys <- subset(
        period_key_info,
        freq %in% frequencies & year >= min_year
      )$key
      if (length(period_keys) < nrow(period_key_info)) {
        period_filter <- list(Perioden = period_keys)
      } else {
        period_filter <- NULL
      }
    } else {
      # min_years used, but not frequencies
      years <- as.character(available_years[available_years >= min_year])
      period_filter <- list(Perioden = cbsodataR::has_substring(years))
    }
  } else if (filter_freqs) {
    # only filter on frequencies
    freqs_cbs <- subset(freq_table, freq %in% frequencies)$freq_cbs
    period_filter <- list(Perioden = cbsodataR::has_substring(freqs_cbs))
  } else {
    period_filter <- NULL
  }

  return(period_filter)
}
