#' Edit timeseries codes
#' 
#' @param ts_code_file the name of a file where the timeseries coding is
#' stored.  This file does not have to exist yet. If it does exist, then it 
#' should be an \code{rds} file containing a \code{ts_code} object.
#' @param use_browser if \code{TRUE} (the default), then display the graphical 
#' user interface in the browser. Otherwise the RStudio viewer is used.
#' @param browser a character vector specifying the path of the browser. Specify
#' \code{"default"} to use the default browser. The approach used when this
#' argument has not been specified depends on the operating system. For non-Windows 
#' operating systems, the default browser is also used if argument 
#' \code{browser} has not been specified. For Windows, a different approach is 
#' used because the Shiny app does not work well in the Internet Explorer. 
#' The function tries to find the location of Chrome or FireFox and if the search
#' is succesful then this browser is used. Otherwise an error is issued.
#' @param debug a logical. If \code{TRUE}, then use the debugging mode
#'  (only for developpers)
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @import shiny
#' @import shinyjqui
#' @importFrom utils packageVersion
#' @importFrom utils packageName
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy show_modal_progress_line update_modal_progress 
#' @importFrom shinybusy remove_modal_progress
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE, browser,
                         debug = FALSE, base_url = NULL) {
  
  if (file.exists(ts_code_file)) {
    
    ts_code <- readRDS(ts_code_file)
    ts_code <- convert_ts_code(ts_code)
    
    if (is.null(ts_code)) {
      stop(paste("File", ts_code_file, "does not contain a ts_code object"))
    }
    
  } else {
    
    ts_code <- create_ts_code()
    
  }
  
  CBS_ORDER <- "Original CBS order"
  SELECTED_FIRST_ORDER <- "Selected first"
  
  # create a vector with table ids and a named vector with table descriptions
  if (length(ts_code) > 0) {
    table_ids <- names(ts_code)
    short_titles <- sapply(ts_code, FUN = function(x) return(x$short_title))
    table_descs <- get_table_description(table_ids, short_titles)
  } else {
    table_ids <- character(0)
    table_descs  <- character(0)
  }
  
  ui <- fluidPage(
    includeCSS(system.file("css", "cbsots.css", package = packageName())),
    shinyjs::useShinyjs(),
    headerPanel('CBS Timeseries Coding'),
    sidebarPanel(
      # the following tag is a workaround for a problem with the actionButton:
      # the button is still highlighted when clicked
      tags$script(HTML("
        $(document).ready(function() {
          $('.btn').on('click', function(){$(this).blur()});
        })
        ")),
      h3("Open Existing Code Table"),
      "You can enter a search query in the text field below.",
      "When necessary, use Backspace to erase the text field.",
      p(),
      selectInput("table_desc", label = NULL, 
                  choices = create_table_choices(table_descs)),
      p(),
      h3("Create new code table"),
      p(),
      actionButton("new_table", "New table"),
      p(),
      h3("Delete Code Table"),
      actionButton("delete_table", "Delete table"),
      p(),
      h3("Order Code Table"),
      "Select an order type below",
      "Press reorder to reorder after changing the table",
      p(),
      fluidRow(
        column(5, selectInput("order_table", label = NULL,
                              choices = c(CBS_ORDER, SELECTED_FIRST_ORDER), 
                              width = "100%")),
        column(1, actionButton("reorder", "Reorder"), offset = 1)
      ),
      p(),
      h3("Update Table(s)"),
      "Updated Keys and Titles with recent information on the CBS website",
      p(),
      fluidRow(
        column(5, actionButton("update_table", "Update This Table")),
        column(1, actionButton("update_all_tables", "Update All Tables"), 
                               offset = 1),
      ),
      h3("Save Code"), 
      paste("Save the Code to File", ts_code_file),
      p(),
      actionButton("save", "Save Codes")
    ),
    mainPanel(
      uiOutput('table_pane')
    )
  )
  
  server <- function(input, output, session) {
    
    session$onSessionEnded(shiny::stopApp)
    
    # register reactive values
    values <- reactiveValues(ts_code = ts_code, table_id = NA_character_,
                             table_desc = NA_character_,
                             table_ids = table_ids,
                             table_descs = table_descs,
                             table_desc_stack = character(0),
                             table_open = FALSE,
                             table_present = length(ts_code) > 0)
    #
    # local functions
    #
    
    get_order_type <- function(dimension) {
      has_cbs_order <- values$ts_code[[values$table_id]]$cbs_key_order[[dimension]]
      if (has_cbs_order) {
        return(CBS_ORDER)
      } else {
        return(SELECTED_FIRST_ORDER)
      }
    }
    
    observeEvent(input$table_desc, {
      
      if (debug) {
        cat(sprintf("table_desc event, table_desc = %s\n", input$table_desc))
      }
      
      if (input$table_desc == "") {
        return()
      }
      
      if (length(values$ts_code) == 0) {
        output$table_pane <- renderUI({return(NULL)})
        return(invisible(NULL))
      }
      
      if (!is.na(values$table_id) && check_duplicates(session, values)) {
        # There are duplicates in the current table. Select original table
        # and return.
        updateSelectInput(session, "table_desc", selected = values$table_desc)
        return()
      }
      
      new_table_id <- get_table_id(input$table_desc)

      if (debug) {
        cat(sprintf("Opening table with id = %s\n", new_table_id))
      }
      
      if (!is.na(values$table_id)) {
        # save ordering current table
        values$ts_code[[values$table_id]]$order <- input$order_input
      }
      
      values$table_id <- new_table_id
      values$table_desc <- values$table_descs[new_table_id]
      values$tab_names <- names(values$ts_code[[new_table_id]]$codes)
      values$table_open <- TRUE
   
      open_table(values, input, output, debug = debug)
      
      values$tab_name <- values$tab_names[1]
      
      updateSelectInput(session, "order_table", selected = get_order_type(1))
    
    })  # table_description_event
    
    observeEvent(values$table_open, {
      if (debug) cat("table_open modified. New value:", values$table_open, 
                     "\n")
      input_ids <- c("update_table", "order_table", "reorder")
      if (values$table_open) {
        fun <- shinyjs::enable
      } else {
        fun <- shinyjs::disable
      }
      invisible(lapply(input_ids, FUN = fun))
      return()
    })
    
    observeEvent(values$table_present, {
      if (debug) cat("table_present modified. New value:", values$table_present, 
                     "\n")
      input_ids <- c("update_all_tables", "delete_table")
      if (values$table_present) {
        fun <- shinyjs::enable
      } else {
        fun <- shinyjs::disable
      }
      invisible(lapply(input_ids, FUN = fun))
      return()
    })
    
    observeEvent(input$new_table, {
      
      new_table_descs <- get_new_table_descs(values$table_ids, base_url)
      
      if (is.null(new_table_descs)) {
        shinyalert("Error", "Error downloading list of tables" , type = "error")
      } else {
        values$new_table_descs <- new_table_descs
        showModal(select_new_table_dialog(new_table_descs, values$table_descs))
      } 
    })
    
    observeEvent(input$new_table_ok, {
      
      new_table_desc <- input$new_table_desc
      if (new_table_desc == "") {
        return()
      }
      new_table_id <- get_table_id(new_table_desc)
      new_table_desc <- values$new_table_descs[new_table_id]
      
      # add new table
      tryCatch({
        
        values$new_table <- create_new_table(new_table_id, base_url)
        values$new_table_id <- new_table_id
        values$new_table_desc <- new_table_desc
  
        base_table_desc <-  input$new_table_base_desc
        if (base_table_desc != "") {
          base_table_id <- get_table_id(base_table_desc)
          base_table <- values$ts_code[[base_table_id]]
          ret <- call_update_table(values$new_table, base_table, new_table_id,
                                   base_table_id)
          if (is.null(ret)) return()
          values$new_table <- ret$new_table
          if (length(ret$warnings) > 0) {
            showWarningsDialog(ret$warnings, "filled_table_ok")
          } else {
            insert_new_table()
            removeModal()
          }
        } else {
          insert_new_table()
          removeModal()
        }
      }, error = function(e) {
        cat("error\n")
        print(e)
        shinyalert("Error", paste("Error while downloading table", 
                                  new_table_id) , 
                   type = "error")
      })
    })
    
    insert_new_table <- function() {
      
      if (debug) {
        cat(sprintf("In function insert_new_table, new_table_id = %s.\n",
                    values$new_table_id))
      }
      
      values$table_ids <- c(values$table_ids, values$new_table_id)
      values$table_descs[values$new_table_id] <- values$new_table_desc
      values$ts_code[[values$new_table_id]]  <- values$new_table
     
      # reorder the tables alphabetically 
      ord <- order(values$table_ids)
      # use create_ts_code, because otherwise the attributes (class and 
      # package version) are lost.
      values$ts_code <- create_ts_code(values$ts_code[ord])
      values$table_ids <- values$table_ids[ord]
      values$table_descs <- values$table_descs[values$table_ids]
      values$table_present <- TRUE
      
      updateSelectInput(session, inputId = "table_desc",
                        choices = create_table_choices(values$table_descs), 
                        selected = values$new_table_desc)
    }
    
    observeEvent(input$filled_table_ok, {
      insert_new_table()
      removeModal()
    })
    
    observeEvent(input$delete_table, {
      
      if (length(values$ts_code) == 0) {
        showModal(modalDialog(
          title = "No tables to delete", "There are no tables to delete",
          easyClose = TRUE)) 
      } else {
        showModal(select_table_dialog("delete_table", "Delete Table", 
                                      values$table_descs))
      }
    })
    
    observeEvent(input$delete_table_ok, {
      
      delete_table_desc <- input$delete_table_desc
      if (delete_table_desc == "") {
        return()
      }
      delete_table_id <- get_table_id(delete_table_desc)
      delete_table_desc <- values$table_descs[delete_table_id]
      values$delete_table_id <- delete_table_id
      
      showModal(modalDialog(
        title = "Confirm",
        HTML(paste0("Table \"", delete_table_desc, 
                    "\" will be permanently deleted",
                    "<br>Are you sure?")),
        footer = tagList(
          modalButton("No"),
          actionButton("delete_table_confirmed", "Yes")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$delete_table_confirmed, {
      
      values$ts_code[[values$delete_table_id]] <- NULL
      
      # update table_ids
      idx <- pmatch(values$delete_table_id, values$table_ids)
      values$table_ids <- values$table_ids[-idx]
      values$table_descs <- values$table_descs[values$table_ids]
      
      choices <- create_table_choices(values$table_descs)                 
      
      if (is.na(values$table_id) || values$delete_table_id == values$table_id) {
        updateSelectInput(session, inputId = "table_desc", choices = choices)
        output$table_pane <- renderUI({return(NULL)})
        values$table_id <- NA_character_
        values$table_open <- FALSE
        selected <- NULL
      } else {
        selected <- values$table_desc
      }
      updateSelectInput(session, inputId = "table_desc", choices = choices,
                        selected = selected)
      
      values$table_present <- length(values$ts_code) > 0
      removeModal()
    })
    
    observeEvent(input$update_table, {
      table_id <- values$table_id
      if (!is.na(table_id)) {
        showModal(modalDialog(
          title = "Confirm",
          HTML(paste0("Do you want to update \"", table_id, "\"",
                      "<br>with recent table information on the CBS website?")),
          footer = tagList(
            modalButton("No"),
            actionButton("update_table_confirmed", "Yes")
          ),
          easyClose = TRUE
        ))
      }
    })
  
    observeEvent(input$update_table_confirmed, {
      removeModal()
      id <- values$table_id
      if (!is.na(id)) {
        new_table <- create_new_table(id, base_url)
        ret <- call_update_table(new_table, values$ts_code[[id]], id, id)
        if (is.null(ret)) return() # something went wrong
        if (length(ret$warnings) > 0) {
          values$update_table_result <- ret$new_table
          showWarningsDialog(ret$warnings, "update_table_ok")
        } else {
          update_table(ret$new_table)
        }
      }
    })
    
    update_table <- function(update_table_result) {
      if (debug) cat("in insert_update_table\n")
      values$ts_code[[values$table_id]] <- update_table_result
      open_table(values, input, output, selected_tab = input$tabsetpanel, 
                 debug = debug)
    }
    
    observeEvent(input$update_table_ok, {
      removeModal()
      update_table(values$update_table_result)
    })
    
    observeEvent(input$update_all_tables, {
      if (length(values$ts_code) > 0) {
        showModal(modalDialog(
          title = "Confirm",
          HTML(paste0("Do you want to update all tables",
                      "<br>with recent table information on the CBS website?")),
          footer = tagList(
            modalButton("No"),
            actionButton("update_all_tables_confirmed", "Yes")
          ),
          easyClose = TRUE
        ))
      }
    })
    
    observeEvent(input$update_all_tables_confirmed, {
      if (debug) cat("Update all tables confirmed\n")
      ids <- names(values$ts_code)
      n_ids <- length(ids)
      removeModal()
      show_modal_progress_line(text = "Updating tables")
      i <- 0
      update_single_table <- function(id) {
        if (debug) cat("updating table ..", id, "\n")
        new_table <- create_new_table(id, base_url)
        ret <- call_update_table(new_table, values$ts_code[[id]], id, id)
        ret$has_warning <- length(ret$warnings) > 0
        ret$warnings <- NULL
        i <<- i + 1
        update_modal_progress(i / n_ids)
        return(ret)
      }
      update_all_tables_result <- sapply(names(values$ts_code), 
                                         FUN = update_single_table,
                                         simplify = FALSE)
      has_warning <- sapply(update_all_tables_result, 
                            FUN = function(x) return(x$has_warning))
      warning_ids <- names(has_warning[has_warning])
      remove_modal_progress()
      if (length(warning_ids) > 0) {
        values$update_all_tables_result <- update_all_tables_result
        wmsg <- paste("For tables", paste(warning_ids, collapse = ", "),
                      "some old keys do not match perfectly with new keys.\n",
                      "Check the match reports in directory 'match_reports'.")
        wmsg <- strwrap(wmsg, width = 80)
        wmsg <- paste(wmsg, collapse = "\n")
        showWarningsDialog(wmsg, "update_all_tables_ok")
      } else {
        update_all_tables(update_all_tables_result)
      }
      return()
    })
    
    observeEvent(input$update_all_tables_ok, {
      if (debug) cat("Update all tables ok\n")
      removeModal()
      update_all_tables(values$update_all_tables_result)
    })
    
    update_all_tables <- function(update_all_tables_result) {
      new_ts_code <- sapply(update_all_tables_result,
                            FUN = function(x) return(x$new_table),
                            simplify = FALSE)
      values$ts_code <- create_ts_code(new_ts_code)
      if (!is.na(values$table_id)) {
        # reopen the table which has been updated
        open_table(values, input, output, debug = debug)
      }
    }
    
    observeEvent(input$tabsetpanel, {
      cat("\ntabsetpanel event\n")
      # a new tab has been selected
    
      if (debug) cat(sprintf("tab selection changed, new selected tab =  %s.\n", 
                         input$tabsetpanel))

      name <- input$tabsetpanel
      tab_id <- name
      # het is noodzakelijk om 
      
      x <- codetable(values$ts_code[[values$table_id]]$codes[[name]])
      print(x)
      #output[["hot"]] <- renderPrint("piet") 
      output[["hot"]] <- renderCodetable(x)
    
      return()
      
      # first clean all tabs, the table for the selected tab will be recreated.
      make_empty_table <- function(name) {
        hot_id <- get_hot_id(values$table_id, name)
        
        output[[hot_id]] <- NULL
        return()
      }
      lapply(values$tab_names, FUN = make_empty_table)
    
      name <- input$tabsetpanel
      
      # Update the order_table input if necessary, and reorder the table if
      # the table is ordered with SELECTED_FIRST_ORDER
      current_type <- get_order_type(name)
      if (input$order_table != current_type) {
        # Update the select input for order_table. Note that this will also 
        # cause an "order_table" event, so that funtion reorder_table() will be 
        # called. If current_type == SELECTED_FIRST_ORDER then the table will
        # actually be reordered.
        updateSelectInput(session, "order_table", selected = current_type)
      } else if (current_type == SELECTED_FIRST_ORDER) {
        reorder_table()
      }
      
      # When a new tab has been selected, we want to re-render the table, 
      # because sometimes handsontable does not render the table correctly when 
      # the tab selection changes. 
      # This may be a problem with suspend, try this option.
      # If current_type == SELECTED_ORDER, then the tables have already been 
      # re-rendered by function reorder_table() (see code above).
      if (current_type != SELECTED_FIRST_ORDER) {
        tab <- values$ts_code[[values$table_id]]$codes[[name]]
        hot_id <- get_hot_id(values$table_id, name)
        output[[hot_id]] <- renderCodetable(codetable(tab))
      }
    })
  
    # Reorder the table in the current tab
    reorder_table <- function() {

      name <- input$tabsetpanel
      if (is.null(name)) {
        # this happens when the app starts
        return()
      }
      
      new_type <- input$order_table
      current_type <- get_order_type(name)
      if (current_type != new_type || new_type == SELECTED_FIRST_ORDER) {
        # for the SELECTED_FIRST_ORDER we always want to reorder since the
        # selection may have changed. This is not necessary for CBS_ORDER.
        hot_id <- get_hot_id(values$table_id, name)        
        if (debug) cat(sprintf("Reordering table %s.\n", hot_id))
        tab <- values$ts_code[[values$table_id]]$codes[[name]]
        tab <- order_code_rows(tab, cbs_order = new_type == CBS_ORDER)
        values$ts_code[[values$table_id]]$codes[[name]] <- tab
        output[[hot_id]] <- renderCodetable(codetable(tab))
      }
      
      return()
    }
    
    
    observeEvent(input$order_table, {
      
      if (debug) cat(sprintf("order_table event (tab = %s).\n", 
                             input$tabsetpanel))
      
      name <- input$tabsetpanel
      if (is.null(name)) {
        # this happens when the app starts
        return()
      }
      
      # first reorder table
      reorder_table()
      
      # and then update cbs_key_order
      values$ts_code[[values$table_id]]$cbs_key_order[[name]] <- 
                                             input$order_table == CBS_ORDER
      
      if (debug) {
        cat("Updating cbs_key_order, new value = \n")
        print(values$ts_code[[values$table_id]]$cbs_key_order)
      }

    })
    
    observeEvent(input$reorder, {
      if (debug) cat(sprintf("reorder event (tab = %s).\n", input$tabsetpanel))
      reorder_table()
    })
    
    observeEvent(input$save, {
      
      if (check_duplicates(session, values)) return()
      
      # reorder the table in the current tab
      reorder_table()
      
      # save ordering  
      values$ts_code[[values$table_id]]$order <- input$order_input
      if (debug) {
        cat("saving ts_code\n")
        print(values$ts_code)
      }
      
      saveRDS(values$ts_code, file = ts_code_file)
    })
  }
  
  app_list <- list(ui = ui, server = server)
  
  
  if (use_browser) {
    old_browser <- options("browser")
    tryCatch({
      options(browser = find_browser(browser))
      runApp(app_list, launch.browser = TRUE)
    }, finally = {
      options(browser = old_browser$browser)
    })
  } else {
    runApp(app_list)
  }
  
  
  return(invisible(NULL))
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
