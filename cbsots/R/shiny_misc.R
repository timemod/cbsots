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

# Returns a named character vector with table descriptions, based on the table ids
# and short titles. The names are the table ids.
get_table_description <- function(ids, short_titles) {
  ret <- paste(ids, short_titles, sep = " - ")
  names(ret) <- ids
  return(ret)
}

# Convert a string with a table description to a table id. The table id is the 
# text before the first " - "  substring.  NOTE: we assume here that the
# table id does not contain a substring " - ".
#' @importFrom stringr str_split
get_table_id <- function(table_desc) {
  ret <- str_split(table_desc, " - ", n = 2)
  return(unlist(ret)[1])
}

check_duplicates <- function(session, values) {
  ts_code <-  values$ts_code[[values$table_id]]$codes
  for (name in values$tab_names) {
    tab <- ts_code[[name]]
    # first check for duplicate keys in the selected entries\
    keys <- tab$Key[tab$Select]
    if (anyDuplicated(keys)) {
      dupl <- keys[duplicated(keys)]
      showModal(modalDialog(
        title = "Duplicates in selected keys",
        HTML(paste0("Duplicates in selected keys of ", name,
                    "<br>Duplicate keys:\n", paste(dupl, collapse = ", "),
                    "<br>Correct before proceeding by deselecting duplicate keys.")),
        easyClose = TRUE
      ))
      return(TRUE)
    }
    # check for duplicate codes for selected entries
    codes <- tab$Code[tab$Select]
    codes <- codes[nchar(codes) > 0]
    if (anyDuplicated(codes)) {
      dupl <- codes[duplicated(codes)]
      showModal(modalDialog(
        title = "Duplicates in code",
        HTML(paste0("Duplicate code for the selected keys of ", name,
                    "<br>Duplicate codes:\n", paste(dupl, collapse = ", "),
                    "<br>Correct before proceeding.")),
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
get_new_table_descs <- function(old_table_ids, base_url) {
  
  tryCatch({
    
    # select does not work anymore
    if (is.null(base_url)) {
      table_info <- cbs_get_toc(Language = "nl")
    } else {
      table_info <- cbs_get_toc(Language = "nl", base_url = base_url)
    }
    table_info <- table_info[ , c("Identifier", "ShortTitle")]
    
    new_table_ids <- setdiff(table_info$Identifier, old_table_ids)
    table_info <- table_info[table_info$Identifier %in% new_table_ids, ]
    table_info <- table_info[order(table_info$Identifier), ]
    
    new_table_descs <- get_table_description(table_info$Identifier, 
                                             table_info$ShortTitle)
    return(new_table_descs)
  },
  error = function(e) {
    warning("Error when downloading table list")
  }
  )
  
  # error
  return(NULL)
}

create_table_choices <- function(names) {
  return(c("Select a table ..." = "", unname(names)))
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

get_hot_id <- function(tab_name, tab_names) {
  return("hot")
  #return(paste0("hot_", tab_name))
}

#' @importFrom utils capture.output
call_update_table <- function(table, base_table, table_id, base_table_id) {
  # This function calls update_table and captures warnings. 
  # It returns a list containing the updated table and the captured warnings.
  tryCatch({
    warnings <- character(0)
    dum <- capture.output({
      withCallingHandlers(
        new_table <- update_table(table, base_table, table_id, base_table_id), 
        warning = function(w) {
          warnings <<- c(warnings, w$message)
        }
      )}, type = "message")
    return(list(new_table = new_table, warnings = warnings))
  }, error = function(e) {
    shinyalert("Error", e$message, type = "error")
    return(NULL)
  })
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
