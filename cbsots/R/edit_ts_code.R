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
        column(5, selectInput("table_order", label = NULL,
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
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanelBody("empty", NULL),
        tabPanelBody("tables", 
          htmlOutput("table_title"),
          orderInput(inputId = "dimension_order", 
                     label = paste("Order of dimensions used to create",
                                   "names (drag and drop items to",
                                   "change order):"), 
                     items = "dummy"),
          p(),
          p(),
          tags$div(
            HTML("&#128270;"),
            tags$input(type = "text", id = "search_field",
                       placeholder = "Search ..."),
            tags$button(HTML("&#8249;"), class = "previous round", 
                        id = "prev_button"),
            tags$button(HTML("&#8250;"), class = "next round", 
                        id = "next_button")
          ),
          uiOutput("dimension_tabsetpanel"),
          codetableOutput("hot")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    session$onSessionEnded(shiny::stopApp)
    
    table_present <- length(ts_code) > 0
    
    # register reactive values
    values <- reactiveValues(ts_code = ts_code, table_id = NA_character_,
                             table_desc = NA_character_,
                             table_ids = table_ids,
                             table_descs = table_descs,
                             table_desc_stack = character(0),
                             table_open = FALSE,
                             table_present = table_present,
                             dimension = NA_character_)
    
    # disable some input elements
    input_ids <- c("update_table", "table_order", "reorder")
    if (!table_present) {
      input_ids <- c(input_ids, "update_all_tables", "delete_table")
    }
    invisible(lapply(input_ids, FUN = shinyjs::disable))
    
    ############################################################################
    # internal function in the server function
    ############################################################################
    
    render_hot_table <- function(dimension) {
      tab_data <- values$ts_code[[values$table_id]]$codes[[dimension]]
      output$hot <- renderCodetable(codetable(tab_data))
      values$dimension <- dimension
      return(invisible())
    }
    
    get_order_type <- function(dimension) {
      has_cbs_order <- values$ts_code[[values$table_id]]$cbs_key_order[[dimension]]
      if (has_cbs_order) {
        return(CBS_ORDER)
      } else {
        return(SELECTED_FIRST_ORDER)
      }
    }
   
    open_table <- function(selected_dimension = NULL) {
      if (debug) {
        cat(sprintf("\nOpening table_id %s (function open_table()).\n", 
                    values$table_id))
      }
      
      output$table_title <- renderUI(HTML("<h3>Tabel", values$table_desc, "</h3>"))
      
      updateOrderInput(
        session = getDefaultReactiveDomain(),
        inputId = "dimension_order",
        items = values$ts_code[[values$table_id]]$order
      )
      
      # create a tabsetpanel with empty panels
      tabset_panel <- do.call(tabsetPanel, c(
        list(
          id = "dimension",
          selected = selected_dimension
        ),
        lapply(values$dimensions, FUN = tabPanel)
      ))
      output$dimension_tabsetpanel <- renderUI(tabset_panel)
      
      dimension <- selected_dimension
      if (is.null(dimension)) dimension <- values$dimensions[1]
      render_hot_table(dimension)   
      
      updateSelectInput(session, "table_order", 
                        selected = get_order_type(dimension))
      
      if (debug) cat("The table has been opened\n\n")
      
      return(invisible())
    }

    # Order the ts code for table value$table_id and dimension value$dimension
    order_ts_code <- function() {
      table_id <- values$table_id
      dim <- values$dimension
      tab_data <- values$ts_code[[table_id]]$codes[[dim]]
      cbs_order <- get_order_type(dim) == CBS_ORDER
      tab_data_ordered <- order_code_rows(tab_data, cbs_order = cbs_order)
      values$ts_code[[table_id]]$codes[[dim]] <- tab_data_ordered
      return(!identical(tab_data$Key, tab_data_ordered$Key))
    }
    
    # Store the handsontable data in values$ts_code.
    # If reorder = TRUE, then the data is also reordered for the order type
    # SELECTED_FIRST_ORDER.
    store_hot_data <- function(reorder = TRUE) {
      if (is.null(input$hot)) {
        return(invisible()) # this may happen at the very beginning
      }
      if (debug) {
        cat("\nUpdating ts_code with data of hot table (function store_hot_data).\n")
      }
      hot_data <- convert_codetable(input$hot)
      if (is.null(hot_data)) {
        shinyalert("Error", "Internal error: hot data not correct")
        return()
      }
      
      table_id <- values$table_id
      dim <- values$dimension
      
      if (debug) {
        cat("table_id = ", table_id, "\n")
        cat("dimension = ", dim, "\n")
        cat("old values:\n")
        print(head(values$ts_code[[table_id]]$codes[[dim]][, 1:3]))
        cat("\nNew values:\n")
        print(head(hot_data[, 1:3]))
      }
      
      # check hot data
      data_old <- values$ts_code[[table_id]]$codes[[dim]][, 1:4]
      if (!identical(data_old$Key, hot_data$Key) ||
          !identical(data_old$Title, hot_data$Title)) {
        shinyalert("Error", "Internal Error: hot data not correct")
        return()
      }
      
      # Store hot data in values$ts_code
      values$ts_code[[table_id]]$codes[[dim]][, 1:4] <- hot_data[, 1:4]
      
      # Reorder data, this is only necessary for SELECTED_FIRST_ORDER
      # (for CBS_ORDER the table is already in the correct order).
      if (reorder && input$table_order == SELECTED_FIRST_ORDER) {
        order_ts_code()
      }
      if (debug) cat("ts_code has been updated\n\n")
      
      return(invisible())
    }
    
    # Reorder the table that is currently displayed.
    reorder_table <- function() {
      if (debug) cat("\nIn reorder_table\n\n")
      store_hot_data(reorder = FALSE)
      dimension <- values$dimension
      if (order_ts_code()) {
        if (debug) cat("The table has been reordered\n\n")
        render_hot_table(dimension)
      } else {
        if (debug) cat("The table was already in correct ordering.\n\n")
      }
      return(invisible())
    }
   
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
    
    
    update_table <- function(update_table_result) {
      if (debug) cat("\nIn update_table\n")
      values$ts_code[[values$table_id]] <- update_table_result
      # TODO: is het nodig om de hele tabel opnieuw te openen?
      # Dit is alleen nodig als het aantal en de namen van de dimensies
      # gewijzigd kan zijn. Is dit mogelijk?
      open_table(selected_tab = input$dimension)
    }
    
    ############################################################################
    # observers
    ############################################################################
    
    observeEvent(input$table_desc, {
      if (debug) {
        cat(sprintf("\nA table has been selected, table_desc = %s\n", 
                    input$table_desc))
      }

      if (values$table_open) {
        if (check_duplicates(values$ts_code, values$table_id)) {
          # There are duplicates in the code of the current table, which 
          # the user must correct first. Select original table and return.
          updateSelectInput(session, "table_desc", selected = values$table_desc)
          return()
        }
        
        # update data in hot table
        store_hot_data()
      
        # save ordering current table
        values$ts_code[[values$table_id]]$order <- input$dimension_order
      }
      
      new_table_id <- get_table_id(input$table_desc)

      if (debug) {
        cat(sprintf("Opening table with id = %s\n\n", new_table_id))
      }
      
      dimensions <- names(values$ts_code[[new_table_id]]$codes)
      
      
      values$table_id <- new_table_id
      values$table_desc <- values$table_descs[new_table_id]
      values$dimensions <- names(values$ts_code[[new_table_id]]$codes)
      values$table_open <- TRUE
   
      open_table()
      
      # table_order aanpassen aan de naam van de eerste dimensie
      current_type <- get_order_type(values$dimension)
      updateSelectInput(session, "table_order", selected = current_type)
    }, ignoreInit = TRUE)  # table_description_event
    
    observeEvent(values$table_open, {
      if (debug) cat("\ntable_open modified. New value:", values$table_open, 
                     "\n")
      # Update different action buttons
      input_ids <- c("update_table", "table_order")
      fun <- if (values$table_open) shinyjs::enable else shinyjs::disable
      invisible(lapply(input_ids, FUN = fun))
      if (values$table_open && input$table_order == SELECTED_FIRST_ORDER) {
        shinyjs::enable("reorder")
      } else {
        shinyjs::disable("reorder")
      }
     
      # make the panel with information of the table visible
      selected <- if (values$table_open) "tables" else "empty"
      updateTabsetPanel(inputId = "switcher", selected = selected)

    }, ignoreInit = TRUE)
    
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
    }, ignoreInit = TRUE)
    
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
        if (!is.na(values$table_id)) {
          values$table_id <- NA_character_
          values$table_desc <- NA_character_
          values$dimension <- NA_character_
          values$table_open <- FALSE
        }
      } else {
        updateSelectInput(session, inputId = "table_desc", choices = choices,
                          selected = values$table_desc)
      }
    
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
        # TODO: select the tab that has been selected previously
      }
    }
    
    observeEvent(input$dimension, {
      if (debug) {
        cat(sprintf("\nSelected dimension changed, new dimension =  %s.\n", 
                             input$dimension))
        cat("input$dimension:", input$dimension, "\n")
        cat("values$dimension:", values$dimension, "\n")
      }
      
      if (values$dimension == input$dimension) {
        # this may happen when the table has just been opened
        cat("No action required\n\n")
        return()
      } else {
        cat("\n")
      }

      store_hot_data()
      render_hot_table(input$dimension)
      current_type <- get_order_type(values$dimension)
      updateSelectInput(session, "table_order", selected = current_type)
    })
  
    observeEvent(input$table_order, {
      if (debug) cat(sprintf("table_order event (dimension = %s).\n", 
                             values$dimension))
      dimension <- values$dimension

      # reorder table
      reorder_table()
      
      # update cbs_key_order
      values$ts_code[[values$table_id]]$cbs_key_order[[dimension]] <-
        input$table_order == CBS_ORDER
      
      if (debug) {
        cat("Updating cbs_key_order, new value = \n")
        print(values$ts_code[[values$table_id]]$cbs_key_order)
        cat("\n")
      }
      
      # update reorder button
      reorder_allowed <- input$table_order == SELECTED_FIRST_ORDER
      if (reorder_allowed && values$table_open) {
        shinyjs::enable("reorder")
      } else {
        shinyjs::disable("reorder")
      }      
    }, ignoreInit = TRUE)
    
    observeEvent(input$reorder, {
      if (debug) cat(sprintf("reorder event (dimension = %s).\n", 
                             values$dimension))
      reorder_table()
    })
    
    observeEvent(input$save, {
      
      # TODO: what happend when values$table_id is NA (not a single table open)?
      if (check_duplicates(values$ts_code, values$table_id)) return()
      
      # reorder the table in the current tab
      reorder_table()
      
      # save ordering  
      values$ts_code[[values$table_id]]$order <- input$dimension_order
      if (debug) {
        cat("saving ts_code\n")
        print(values$ts_code)
      }
      
      saveRDS(values$ts_code, file = ts_code_file)
    })
    
    # observeEvent(input$hot, {
    #   cat("\nHot table changed\n")
    # })
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
