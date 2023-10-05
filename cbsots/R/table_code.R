#' @import data.table
table_code <- function(id, base_url = NULL) { 
  
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

# returns all dimensions for the tbl_code  
get_dimensions <- function(tbl_code) {
  return(names(tbl_code$codes))
}

# Order the codes in a table_code object according to cbs_key_order.
order_table_code <- function(tbl_code) {
  dimensions <- get_dimensions(tbl_code)
  tbl_code$codes[dimensions] <- mapply(
    FUN = order_code_rows,
    tbl_code$codes[dimensions],
    tbl_code$cbs_key_order[dimensions],
    SIMPLIFY = FALSE
  )

  return(tbl_code)
}

# Order the rows of a code table in a table_code object
order_code_rows <- function(code,  cbs_order) {
  
  orig_key_order <- code$OrigKeyOrder
  
  if (cbs_order) {
    
    required_order <- orig_key_order
    
  } else {
    
    # selected rows first
    selected <- code$Key[code$Select]
    not_selected <- code$Key[!code$Select]
    selected_ordered <- selected[match(intersect(orig_key_order, selected),
                                       selected)]
    not_selected_ordered <- not_selected[match(intersect(orig_key_order, 
                                                         not_selected), 
                                               not_selected)]
    required_order <- c(selected_ordered, not_selected_ordered) 
  }
  
  order <- match(required_order, code$Key)
  
  code[, 1:4] <- code[order, 1:4]
  
  return(code)
}