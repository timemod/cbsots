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
  return(paste(ids, short_titles, sep = " - "))
}


check_duplicates <- function(session, values) {
  ts_code <-  values$tables[[values$table_id]]$codes
  for (name in values$names) {
    tab <- ts_code[[name]]
    codes <- tab$Code[tab$Select]
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
#' @importFrom cbsodataR cbs_get_toc
get_new_table_ids <- function(old_table_ids, base_url) {
  
  tryCatch({
    
    
    #if (is.null(base_url)) {
    #  table_info <- cbs_get_toc(select = c("Identifier", "ShortTitle"), 
    #                            Language = "nl")
    #} else {
    #  table_info <- cbs_get_toc(select = c("Identifier", "ShortTitle") 
    #                            Language = "nl", base_url = base_url)
    #}
    
    # select does not work anymore
    if (is.null(base_url)) {
      table_info <- cbs_get_toc(Language = "nl")
    } else {
      table_info <- cbs_get_toc(Language = "nl", base_url = base_url)
    }
    table_info <- table_info[ , c("Identifier", "ShortTitle")]
    
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

convert_codetable <- function(table) {
  if (length(table) %% 4 != 0) {
    warning("Internal error: length hot data not divisible by 4")
    return(NULL)
  } else if (length(table) == 0) {
    warning("Internal error: length hot data is 0")
    return(NULL)
  } else {
    data <- matrix(table, ncol = 4, byrow = TRUE)
    colnames(data) <- c("Key", "Select", "Code", "Title")
    data <- as.data.table(data)
    data$Select <- as.logical(data$Select)
    return(data)
  }
}

get_hot_id <- function(table_id, name) {
  return(paste(table_id, name, sep = "_"))
}

call_update_table <- function(table, base_table) {
  # This function calls update_table and captures warnings. 
  # It returns a list containing the updated table and the captured warnings.
  warnings <- character(0)
  dum <- capture.output({
    withCallingHandlers(
    new_table <- update_table(table, base_table), 
    warning = function(w) {
      warnings <<- c(warnings, w$message)
    }
  )}, type = "message")
  return(list(new_table = new_table, warnings = warnings))
}

showWarningsDialog <- function(warnings, ok_button_id) {
  showModal(modalDialog(
    title = "Warning(s):",
    HTML(paste(warnings, collapse = "<br>"),
         HTML("<br>Do you want to continue?")),
    footer = tagList(
      modalButton("No"),
      actionButton(ok_button_id, "Yes")
    ),
    easyClose = TRUE
  ))
}