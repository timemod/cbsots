read_match_report <- function(ts_code, table_id, base_id = table_id) {
  
  table <- ts_code[[table_id]]
 
  if (base_id == table_id) {
    dimensions <-  names(table$codes)
  } else {
    base_tab <- ts_code[[base_id]]
    dimensions <- intersect(names(table$codes), names(base_tab$codes))
  }
 
   prefix <- if (base_id == table_id) {
    table_id
  } else {
    paste0(base_id, "_",  table_id)
  }
  
  filenames <- paste0(prefix, "_", dimensions, ".xlsx")
  filenames <- file.path("match_reports", filenames)
  
  read_file <- function(filename) {
    filename %>% 
      readxl::excel_sheets() %>% 
      purrr::set_names() %>% 
      purrr::map(readxl::read_excel, path = filename)    
  }
  
  ret <- sapply(filenames, FUN = read_file, simplify = FALSE)

  return(ret)
}