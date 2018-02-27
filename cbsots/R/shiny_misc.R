#
# Miscellaneous help functions for the shiny app
#

read_ts_code <- function(filename) {
  
  ts_code <- readRDS(filename)

  if (ts_code$package_version == "0.1") {
    stop("old package") 
  }
    
  if (!inherits(ts_code, "ts_code")) {
    stop(paste("File", filename, "does not contain a ts_code object"))
  }
  
  return(ts_code)
}

# Returns a character vector with table descriptions, based on the table ids
# and short titles.
get_table_description <- function(ids, short_titles) {
  return(paste(ids, "-", short_titles))
}

# Returns the table_id for a table with a table description.
# INPUT:
#   table_desc: the table description (a character)
#   table_ids:  a named character vector with the table ids. the names are the 
#               table descriptions.
# RETURN:
#   the table_id, or NA if an error has occurred
get_table_id <- function(table_desc, table_ids) {

  table_id <- table_ids[table_desc]
 
  # error message
  if (is.na(table_id)) {
    cat("\n*** Internal error in get_table_id ***\n")
    cat("Table \"", table_desc, "\" not found in list of tables\n")
    cat("Current list of tables\n")
    if (length(table_ids) == 0) {
      print(table_ids)
    } else {
      print(as.data.frame(table_ids))
    }
    cat("\n\n")
  }
  
  return(table_id)
}




update_tables <- function(table_id, values, input, debug) {
  
  old <- values$tables[[table_id]]
  
  if (is.null(old)) {
    # this situation occurs when a table has been deleted
    return(invisible(NULL))
  }
  
  # ordering
  values$tables[[table_id]]$order <- input$order_input_order
  
  # tables
  for (name in values$names) {
    orig_key_order <- values$tables[[table_id]]$codes[[name]]$OrigKeyOrde
    values$tables[[table_id]]$codes[[name]][ ,1:4] <- 
      order_code_rows(values[[name]], orig_key_order)
  }
  
  if (!isTRUE(all.equal(old, values$tables[[table_id]]))) {
    values$tables[[table_id]]$last_modified <- Sys.time()
  }
  
  if (debug) {
    cat(sprintf("\n\nSaving current values for table %s\n", table_id))
    print(values$tables[[table_id]])
  }
  
  return(invisible(NULL))
}


check_duplicates <- function(session, values) {
  for (name in values$names) {
    codes <- values[[name]]$Code[values[[name]]$Select]
    if (anyDuplicated(codes)) {
      dupl <- codes[duplicated(codes)]
      showModal(modalDialog(
        title = "Duplicates in code",
        HTML(paste0("Duplicate code for the selected keys of ", name,
                    "<br>Duplicatecodes:\n", paste(dupl, collapse = ", "),
                    "<br>Please correct before proceding.")),
        easyClose = TRUE
      ))
      
      return(TRUE)
    }
  }
  return(FALSE)
}

#  Function get_new_table_ids returns a character vector the ids of tables
#  that are not yet present.
#' @importFrom cbsodataR get_table_list
get_new_table_ids <- function(old_table_ids) {

  table_info <- get_table_list(select = c("Identifier", "ShortTitle"), 
                               Language = "nl")

  new_tables <- setdiff(table_info$Identifier, old_table_ids)
  table_info <- table_info[table_info$Identifier %in% new_tables, ]
  table_info <- table_info[order(table_info$Identifier), ]
  new_table_descriptions <- get_table_description(table_info$Identifier, 
                                                  table_info$ShortTitle)
  
  new_table_ids <- table_info$Identifier
  names(new_table_ids) <- new_table_descriptions
  return(new_table_ids)
}

create_table_choices <- function(names) {
  return(c("Select a table ..." = "", names))
}

convert_codetable <- function(table, colnames) {
  data <- matrix(table, ncol = 4, byrow = TRUE)
  colnames(data) <- colnames
  data <- as.data.table(data)
  data$Select <- as.logical(data$Select)
  return(data)
}

