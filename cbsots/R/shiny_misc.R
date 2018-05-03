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

check_duplicates <- function(session, values) {
  for (name in values$names) {
    codes <- values[[name]]$Code[values[[name]]$Select]
    codes <- codes[nchar(codes) > 0]
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
get_new_table_ids <- function(old_table_ids, base_url) {
  
  tryCatch({
    
    if (is.null(base_url)) {
      table_info <- get_table_list(select = c("Identifier", "ShortTitle"), 
                                   Language = "nl")
    } else {
      table_info <- get_table_list(select = c("Identifier", "ShortTitle"), 
                                   Language = "nl", 
                                   base_url = base_url)
    }
    
    new_tables <- setdiff(table_info$Identifier, old_table_ids)
    table_info <- table_info[table_info$Identifier %in% new_tables, ]
    table_info <- table_info[order(table_info$Identifier), ]
    new_table_descriptions <- get_table_description(table_info$Identifier, 
                                                    table_info$ShortTitle)
    
    new_table_ids <- table_info$Identifier
    names(new_table_ids) <- new_table_descriptions
    return(new_table_ids)
  },
  error = function(e) {
    warning("Error when downloading table list")
  }
  )
  
  # error
  return(NULL)
}

create_table_choices <- function(names) {
  return(c("Select a table ..." = "", names))
}

convert_codetable <- function(table, colnames) {
  
  data <- matrix(table, ncol = 5, byrow = TRUE)

  # extract dimesion and table_id  and remove the corresponding columns
  table_id <- data[1, 5]
  data <- data[, 1:4, drop = FALSE]
  
  colnames(data) <- colnames
  data <- as.data.table(data)
  data$Select <- as.logical(data$Select)
  
  return(list(table_id = table_id, data = data))
}


get_hot_id <- function(table_id, name) {
  return(paste(table_id, name, sep = "_"))
}
