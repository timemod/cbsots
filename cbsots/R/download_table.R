#' @importFrom cbsodataR cbs_get_data
#' @importFrom cbsodataR cbs_get_meta
download_table <- function(id, raw_cbs_dir, code, min_year, frequencies,  
                           base_url, download_all_keys) {
  
  cat(paste("Downloading table", id, "...\n"))
  
  # first download the meta data, which is needed to create the filters
  # for downloading the table data
  if (is.null(base_url)) {
    meta <- cbs_get_meta(id, cache = TRUE)
  } else {
    meta <- cbs_get_meta(id, cache = TRUE, base_url = base_url)
  }
  check_language(meta)
  cbs_code <- get_cbs_code(meta)
  code <- check_code(code, cbs_code)
 
  # create a filter for each dimension if necessary
  get_dimension_filter <-function(dimension) {
     if (nrow(cbs_code[[dimension]]) > nrow(code[[dimension]])) {
      filter <- code[[dimension]]$Key
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
  
  if (!is.null(min_year) || !is.null(frequencies)) {
    period_keys <- get_period_keys(meta, min_year, frequencies)
    if (length(period_keys) < nrow(meta$Perioden)) {
      filters <- c(list(Perioden = period_keys), filters)
    }
  }
  
  if (length(filters) > 0) {
    filters <- filters[sapply(filters, FUN = function(x) {!is.null(x)})]   
    cat("Filters:\n")
    print(filters)
  }
  
  # TODO: use argument select to select only the topics that we need.
  # problem: this may lead to a too long url. Maybe we should
  # keep downloading all columns
  arguments <- c(list(id = id,  dir = file.path(raw_cbs_dir, id)), filters)
  if (!is.null(base_url)) {
    arguments <- c(arguments, list(base_url = base_url))
  }
  
  data <- do.call(cbs_get_data, arguments)
  
  # convert factor columns to character columns
  data <- as.data.table(lapply(data, FUN = function(x) {
    if (is.factor(x)) as.character(x) else x
  }))
  
  return(list(meta = meta, data = data, cbs_code = cbs_code))
}