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
    filters <- sapply(dimensions, FUN = get_dimension_filter, simplify = FALSE)
  } else {
    filters <- list()
  }
  
  period_filter <- get_period_filter(meta$Perioden$Key, 
                                     frequencies = frequencies, 
                                     min_year = min_year)
  filters <- c(period_filter, filters)
  
  if (length(filters) > 0) {
    filters <- filters[sapply(filters, FUN = function(x) {!is.null(x)})]   
    cat("Filters:\n")
    print(filters)
  }
  
  # TODO: use argument select to select only the topics that we need?
  # Problem: this may lead to a too long url. Therefore try to estimate the 
  # length of the url and keep the length accordingly.
  arguments <- c(list(id = id,  dir = file.path(raw_cbs_dir, id)), filters,
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
    filter_year <- min(available_years) < min_year
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
    freqs_cbs <- subset(freq_table, freq %in% frequencies)$"freq_cbs"
    period_filter <- list(Perioden = cbsodataR::has_substring(freqs_cbs))
  } else {
    period_filter <- NULL
  }

  return(period_filter)
}
