#' Edit timeseries codes
#' 
#' @param ts_code_file the name of a file where the timeseries coding is
#' stored.  This file does not have to exist yet. If it does exist, then it 
#' should be an \code{rds} file containing a \code{ts_code} object.
#' @param use_browser if \code{TRUE}, then display the graphical user interface
#'  in the browser. Otherwise the RStudio viewer is used.
#' @param debug a logical. If \code{TRUE}, then use the debugging mode
#'  (only for developpers)
#' @import shiny
#' @import shinyjqui
#' @importFrom utils packageVersion
#' @importFrom utils packageName
#' @importFrom shinyalert shinyalert
#' @importFrom shinyalert useShinyalert
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE, 
                         debug = FALSE) {
  
  if (file.exists(ts_code_file)) {
    
    ts_code <- readRDS(ts_code_file)
    ts_code <- convert_ts_code(ts_code)
    
    if (!inherits(ts_code, "ts_code")) {
      stop(paste("File", ts_code_file, "does not contain a ts_code object"))
    }
    
    
  } else {
  
      ts_code <-  structure(list(package_version = packageVersion("cbsots"),
                            table_code = list()), class = "ts_code")
  }
  
  tables <- ts_code$table_code
  
  CBS_ORDER <- "Original CBS order"
  SELECTED_FIRST_ORDER <- "Selected first"
  
  # create a named character vector with table ids. The names are 
  # the table descriptions.
  if (length(tables) > 0) {
    short_titles <- sapply(tables, FUN = function(x) return(x$short_title))
    table_ids <- names(tables)
    names(table_ids) <- get_table_description(table_ids, short_titles)
  } else {
    table_ids <- character(0)
  }

  ui <- fluidPage(
    
    includeCSS(system.file("css", "cbsots.css", package = packageName())),
    
    useShinyalert(),  # Set up shinyalert
    
    headerPanel('CBS Timeseries Coding'),
    sidebarPanel(
      # the following tag is a workaround for a problem with the actionButton:
      # the button is still highlighted when clicked
      tags$script(HTML("
        $(document).ready(function() {
          $('.btn').on('click', function(){$(this).blur()});
        })
        ")),
      h3("Open existing code table"),
      "You can enter a search query in the text field below.",
      "When necessary, use Backspace to erase the text field.",
      p(),
      selectInput("table_desc", label = NULL, 
                 choices = create_table_choices(names(table_ids))),
      p(),
      h3("Create new code table"),
      p(),
      actionButton("new_table", "New table"),
      p(),
      h3("Delete code table"),
      actionButton("delete_table", "Delete table"),
      p(),
      h3("Order code tabel"),
      "Select an order type below",
      "Press reorder to reorder after chaning the table",
      p(),
      fluidRow(
        column(5, selectInput("order_table", label = NULL,
                    choices = c(CBS_ORDER, SELECTED_FIRST_ORDER), width = "100%")),
        column(1, actionButton("reorder", "Reorder"), offset = 1)
      ),
      p(),
      h3("Save code"), 
      paste("Save the code to file", ts_code_file),
      p(),
      actionButton("save", "Save codes")
    ),
    mainPanel(
      uiOutput('tabel')
    )
  )
  
  server <- function(input, output, session) {
    
  
    session$onSessionEnded(shiny::stopApp)
    
    # register reactive values
    values <- reactiveValues(tables = tables, table_id = NA_character_,
                             table_desc = NA_character_,
                             table_ids = table_ids,
                             table_desc_stack = character(0))
    #
    # local functions
    #
    get_order_type <- function(table_id, name) {
      isolate({
        orig_key_order <- values$tables[[table_id]]$codes[[name]]$OrigKeyOrder
        if (identical(values[[name]]$Key, orig_key_order)) {
          type <- CBS_ORDER
        } else {
          type <- SELECTED_FIRST_ORDER
        }
      })
      return(type)
    }
    
    observeEvent(input$table_desc, {
      
      if (input$table_desc == "") {
        return()
      }
      
      if (length(values$tables) == 0) {
        output$tabel <- renderUI({return(NULL)})
        return(invisible(NULL))
      }
      
      # check for duplicates in current table, otherwise don't change
      if (!is.null(values$table_id)) {
        if (check_duplicates(session, values)) {
          updateSelectInput(session, "table_desc", selected = values$table_desc)
          return()
        }
      }

      open_table(input$table_desc, values, input, output, debug)
      
      current_order <- get_order_type(values$table_id, "Topic")
      updateSelectInput(session, "order_table", selected = current_order)
      
      
    })  # table_description_event
    
    
    observeEvent(input$new_table, {
      
      values$new_table_ids <- get_new_table_ids(values$table_ids)
      
      if (is.null(values$new_table_ids)) {
        shinyalert("Error", "Error downloading list of tables" , type = "error")
      } else {
        showModal(select_table_dialog("new_table", "New Table", 
                                    names(values$new_table_ids)))
      } 
    })
    
    observeEvent(input$new_table_ok, {
      
      new_table_desc <- input$new_table_desc
      if (new_table_desc == "") {
        return()
      }
      new_table_id <- get_table_id(new_table_desc, values$new_table_ids)
      if (is.na(new_table_id)) {
        return(invisible(NULL))
      }
      
      # additional test
      if (new_table_id %in% values$table_ids) {
        cat("\n*** Internal error in get_table_id ***\n")
        cat("Table \"", new_table_desc, "\" already present")
        cat("Current list of tables\n")
        if (length(values$table_ids) == 0) {
          print(values$table_ids)
        } else {
          print(as.data.frame(values$table_ids))
        }
        cat("\n\n")
        return(invisible(NULL))
      }
      
      # add new table
      tryCatch({
        
        values$tables[[new_table_id]] <- create_new_table(new_table_id)
        values$table_ids[new_table_desc] <- new_table_id
      
        # reorder the tables alphabetically
        ord <- order(values$table_ids)
        values$tables <- values$tables[ord]
        values$table_ids <- values$table_ids[ord]
      
        updateSelectInput(session, inputId = "table_desc",
                          choices = create_table_choices(names(values$table_ids)), 
                          selected = new_table_desc)
      
        removeModal()
      }, error = function(e) {
        shinyalert("Error", paste("Error while downloading table", 
                   new_table_id) , 
                   type = "error")
      })
    })
    
  
    observeEvent(input$delete_table, {
      
      if (length(values$tables) == 0) {
        showModal(modalDialog(
          title = "No tables to delete", "There are no tables to delete",
          easyClose = TRUE)) 
      } else {
        showModal(select_table_dialog("delete_table", "Delete Table", 
                                      names(values$table_ids)))
      }
    })
    
    observeEvent(input$delete_table_ok, {
      
      delete_table_desc <- input$delete_table_desc
      if (delete_table_desc == "") {
        return()
      }
      delete_table_id <- get_table_id(delete_table_desc, values$table_ids)
      if (is.na(delete_table_id)) {
        return(invisible(NULL))
      }
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
      
      values$tables[[values$delete_table_id]] <- NULL
      
      # update table_ids
      idx <- pmatch(values$delete_table_id, values$table_ids)
      values$table_ids <- values$table_ids[-idx]

      
      choices <- create_table_choices(names(values$table_ids))                 
      
      if (is.na(values$table_id) || values$delete_table_id == values$table_id) {
        updateSelectInput(session, inputId = "table_desc", choices = choices)
        output$tabel <- renderUI({return(NULL)})
        values$table_id <- NA_character_
        values$table_desc <- NA_character_
      } else {
        updateSelectInput(session, inputId = "table_desc", choices = choices,
                            selected = values$table_desc)
      }

      
      removeModal()
    })
    
    observeEvent(input$selected_tab, {
      # if a new tab has been seleced, check if we need to adapt the
      # order_table input
      name <- input$selected_tab
      current_type <- get_order_type(values$table_id, name)
      selected <- input$order_table
      if (selected != current_type) {
        updateSelectInput(session, "order_table", selected = current_type)
      }
    })
    
    
    observeEvent(input$order_table, {
      
      name <- input$selected_tab
      if (is.null(name)) {
        # this happens when the app starts
        return()
      }
      
      if (input$order_table == CBS_ORDER) {
        type <- "cbs"
      } else {
        type <- "selected_first" 
      }
      
      current_type <- get_order_type(values$table_id, name)
      if (current_type != type) {
        orig_key_order <- values$tables[[values$table_id]]$codes[[name]]$OrigKeyOrder
        values[[name]] <- order_code_rows(values[[name]], orig_key_order,
                                        type = type)
        output[[name]] <- renderCodetable(codetable(isolate(values[[name]])))
      }
    })
    
    observeEvent(input$reorder, {
      
      # TODO: combine code with observeEvent order_table
      name <- input$selected_tab
      if (is.null(name)) {
        # this happens when the app starts
        return()
      }
      
      if (input$order_table == CBS_ORDER) {
        type <- "cbs"
      } else {
        type <- "selected_first" 
      }
      
      current_type <- get_order_type(values$table_id, name)
      if (current_type != type) {
        orig_key_order <- values$tables[[values$table_id]]$codes[[name]]$OrigKeyOrder
        values[[name]] <- order_code_rows(values[[name]], orig_key_order,
                                          type = type)
        output[[name]] <- renderCodetable(codetable(isolate(values[[name]])))
      }
      
    })
    
    
 
      
    observeEvent(input$save, {
      
      if (check_duplicates(session, values)) return()
      
      update_tables(values$table_id, values, input, debug)
      
      ts_code <-  structure(list(package_version = packageVersion("cbsots"),
                            table_code = values$tables),
                            class = "ts_code")
      if (debug) {
        cat("saving table_codes\n")
        print(ts_code)
      }
      
      saveRDS(ts_code, file = ts_code_file)
    })
  }
  
  app_list <- list(ui = ui, server = server)
  
  if (use_browser) {
    runApp(app_list, launch.browser = TRUE)
  } else {
    runApp(app_list)
  }
    
  return(invisible(NULL))
}
