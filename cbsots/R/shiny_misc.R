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

get_table_descs <- function(ts_code) {
  table_ids <- names(ts_code)
  short_titles <- sapply(ts_code, FUN = function(x) return(x$short_title))
  return(get_table_description(table_ids, short_titles))
}

# Convert a string with a table description to a table id. The table id is the 
# text before the first " - "  substring.  NOTE: we assume here that the
# table id does not contain a substring " - ".
#' @importFrom stringr str_split
get_table_id <- function(table_desc) {
  ret <- str_split(table_desc, " - ", n = 2)
  return(unlist(ret)[1])
}

check_duplicates <- function(ts_code, table_id, dimension) {
  ts_code <- ts_code[[table_id]]$codes
  tab <- ts_code[[dimension]]
  # first check for duplicate keys in the selected entries
  keys <- tab$Key[tab$Select]
  if (anyDuplicated(keys)) {
    dupl <- keys[duplicated(keys)]
    showModal(modalDialog(
      title = "Duplicates in selected keys",
      HTML(paste0(
        "Duplicates in selected keys of ", dimension,
        "<br>Duplicate keys:\n", paste(dupl, collapse = ", "),
        "<br>Correct before proceeding by deselecting duplicate keys."
      )),
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
      HTML(paste0(
        "Duplicate code for the selected keys of ", dimension,
        "<br>Duplicate codes:\n", paste(dupl, collapse = ", "),
        "<br>Correct before proceeding."
      )),
      easyClose = TRUE
    ))
    return(TRUE)
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

#' @importFrom shinybusy show_modal_progress_line update_modal_progress 
perform_update_all_tables <- function(ts_code, base_url, debug) {
  
  show_modal_progress_line(text = "Updating tables")
  
  ids <- names(ts_code)
  n_ids <- length(ids)
  
  i <- 0
  update_single_table <- function(id) {
    if (debug) cat("Updating table ..", id, "\n")
    new_table <- create_new_table(id, base_url)
    ret <- call_update_table(new_table, ts_code[[id]], id, id)
    ret$has_warning <- length(ret$warnings) > 0
    ret$warnings <- NULL
    i <<- i + 1
    update_modal_progress(i / n_ids)
    return(ret)
  }
  result <- sapply(ids, FUN = update_single_table, simplify = FALSE)
  
  remove_modal_progress()
  
  has_warning <- sapply(result, FUN = function(x) return(x$has_warning))
  warning_ids <- names(has_warning[has_warning])
  
  return(list(result = result, warning_ids = warning_ids))
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

find_browser <- function(browser) {
  
  if (!missing(browser)) {
    if (browser == "default") {
      return(options("browser")$browser)
    } else if (!file.exists(browser)) {
      stop(sprintf("Executable %s does not exist.\n", browser))
    }
  }
  
  if (.Platform$OS.type != "windows") {
    return(options("browser")$browser)
  } else {
    paths <- c("C:/progs/Google/Chrome/Application/chrome.exe",
               "D:/progs/Google/Chrome/Application/chrome.exe",
               "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
               "c:/Program Files/Mozilla Firefox/firefox.exe")
    
    for (path in paths) {
      if (file.exists(path)) {
        return(path)
      }
    }
    stop("Unable to find Chrome or FireFox on Windows.\n",
         "Use argument use_browser = FALSE or specify the path of the",
         " browser with argument browser.")
  }
}
