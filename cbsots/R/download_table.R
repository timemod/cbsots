download_table <- function(id, raw_cbs_dir, code, selected_code, min_year, 
                           frequencies, base_url, download_all_keys) {
  
  cat(paste("Downloading table", id, "...\n"))
  
  # first download the meta data, which is needed to create the filters
  # for downloading the table data
  meta <- download_meta(id, base_url = base_url)

  cbs_code <- get_cbs_code(meta)

  check_unknown_keys(id, selected_code, cbs_code)
  check_code(id, code, selected_code, cbs_code, downloaded = TRUE)

  filters <- get_download_filters(id,
    selected_code = selected_code,
    cbs_code = cbs_code,
    frequencies = frequencies,
    min_year = min_year,
    period_keys = meta$Perioden$Key,
    base_url = base_url,
    download_all_keys = download_all_keys
  )
  arguments <- c(list(id = id, dir = file.path(raw_cbs_dir, id)), filters,
                 list(cache = TRUE, typed = TRUE))

  
  if (!is.null(base_url)) {
    arguments <- c(arguments, list(base_url = base_url))
  }
  
  data <- do.call(cbsodataR::cbs_download_table, arguments)
 
  return(invisible(NULL))
}

