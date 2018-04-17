download_table <- function(id, raw_cbs_dir, code, min_year, na_strings) {
  
  cat(paste("Downloading table", id, "...\n"))
  
  # first download the meta data, which is needed to create the filters
  # for downloading the table data
  meta <- get_meta(id, cache = TRUE)
  
  check_language(meta)
  cbs_code <- get_cbs_code(meta)
  code <- check_code(code, cbs_code)
 
  # create a filter for each dimension if necessary
  get_dimension_filter <-function(dimension) {
    nkey <- nrow(code[[dimension]])
    # Temporary solution: do not use filter if there are more than 20 keys if 
    # min_year has been specified. If there are more than 20 keys, than 
    # sometimes downloading data fails.
    if ((is.null(min_year) || nkey <= 20) && 
        nrow(cbs_code[[dimension]]) > nrow(code[[dimension]])) {
      filter <- code[[dimension]]$Key
    } else {
      filter <- NULL
    }
    return(filter)
  }

  dimensions <- setdiff(names(code), "Topic")
  filters <- sapply(names(code), FUN = get_dimension_filter, simplify = FALSE)
  
  if (!is.null(min_year)) {
    period_keys <- get_period_keys(meta, min_year)
    if (length(period_keys) < nrow(meta$Perioden)) {
      filters <- c(list(Perioden = period_keys), filters)
    }
  }
  
  if (length(filters) > 0) {
    filters <- filters[sapply(filters, FUN = function(x) {!is.null(x)})]   
    cat("Filters:\n")
    print(filters)
  }
  
  argumenten <- c(list(id = id, recode = FALSE, 
                       dir = file.path(raw_cbs_dir, id)), filters)

  data <- do.call(get_data, argumenten)
  
  # replace na_strings with an empty string, and convert to data.table
  data <- as.data.table(lapply(data, 
                               FUN = function(x) {
                                        ifelse(x %in% na_strings, "", x)
                                      }))
  
  # prevent notes from R CMD check about no visible binding for global variable
  ID <- NULL;
  
  data[, ID := NULL]
  
  return(list(meta = meta, data = data, cbs_code = cbs_code))
}