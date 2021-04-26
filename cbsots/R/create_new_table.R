#' @import data.table 
create_new_table <- function(id, base_url = NULL){
  
  meta <- download_meta(id, base_url = base_url)

  codes <- get_cbs_code(meta)
  
  # add extra columsn needed for the Shiny App
  add_columns <- function(x) {
    x$Code <- ""
    x$Select <- FALSE
    x$OrigKeyOrder <- x$Key
    return(x[, c("Key", "Select", "Code", "Title",  "OrigKeyOrder")])
  }
  codes[] <- lapply(codes, FUN = add_columns)
  
  cbs_key_order <- rep(TRUE,  length(codes))
  names(cbs_key_order) <- names(codes)
  
  return(structure(list(short_title = meta$TableInfos$ShortTitle,
                        order = names(codes), cbs_key_order = cbs_key_order,
                        codes = codes), class = "table_code"))
}