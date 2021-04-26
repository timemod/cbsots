# download meta data for the table from CBS
download_meta <- function(id, base_url) {
  if (is.null(base_url)) {
    meta <- cbsodataR::cbs_get_meta(id, cache = TRUE)
  } else {
    meta <- cbsodataR::cbs_get_meta(id, cache = TRUE, base_url = base_url)
  }
  # convert to data.table:
  meta[] <- lapply(meta, FUN = as.data.table)
  check_language(meta)
  return(meta) 
}