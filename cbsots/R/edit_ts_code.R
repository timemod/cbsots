#' Edit timeseries codes
#' 
#' @param ts_code_file the name of a file where the timeseries coding is
#' stored.  This file does not have to exist yet. If it does exist, then it 
#' should be an \code{rds} file containing a \code{ts_code} object.
#' @param use_browser if \code{TRUE}, then display the graphical user interface
#'  in the browser. Otherwise the RStudio viewer is used.
#' @param debug a logical. If \code{TRUE}, then use the debugging mode
#'  (only for developpers)
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @import shiny
#' @import shinyjqui
#' @importFrom utils packageVersion
#' @importFrom utils packageName
#' @importFrom shinyalert shinyalert
#' @importFrom shinyalert useShinyalert
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE, 
                         debug = FALSE, base_url = NULL) {
  
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
      h3("Order code table"),
      "Select an order type below",
      "Press reorder to reorder after changing the table",
      p(),
      fluidRow(
        column(5, selectInput("order_table", label = NULL,
                              choices = c(CBS_ORDER, SELECTED_FIRST_ORDER), width = "100%")),
        column(1, actionButton("reorder", "Reorder"), offset = 1)
      ),
      p(),
      h3("Update table"),
      "Updated Keys and Titles with recent information on the CBS website",
      p(),
      actionButton("update_table", "Update"),
      h3("Save code"), 
      paste("Save the code to file", ts_code_file),
      p(),
      actionButton("save", "Save codes")
    ),
    mainPanel(
      uiOutput('table_pane')
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
    
    get_order_type <- function(dimension) {
      has_cbs_order <- values$tables[[values$table_id]]$cbs_key_order[[dimension]]
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
      
      if (length(values$tables) == 0) {
        output$table_pane <- renderUI({return(NULL)})
        return(invisible(NULL))
      }
      
      # check for duplicates in current table, otherwise don't change
      if (!is.null(values$table_id)) {
        if (check_duplicates(session, values)) {
          updateSelectInput(session, "table_desc", selected = values$table_desc)
          return()
        }
      }
      
      new_table_desc <- input$table_desc
      new_table_id <- values$table_ids[new_table_desc]
      if (is.na(new_table_id)) {
        warning(paste("Internal error ... Table", new_table_desc, 
                      "not in list of tables"))
        return(invisible(NULL))
      }
      
      if (debug) {
        cat(sprintf("Opening table with id = %s\n", new_table_id))
      }
      
      if (!is.na(values$table_id)) {
        # another table now open
        if (values$table_id == new_table_id) {
          return(invisible(NULL))
        } else {
          # save ordering
          values$tables[[values$table_id]]$order <- input$order_input_order
        }
      }
      
      values$table_id <- new_table_id
      values$table_desc <- new_table_desc
      
      open_table(values, input, output, debug = debug)
      
      # TODO: only do this when the orde type has actually changed, to prevent
      # an extra event.
      updateSelectInput(session, "order_table", selected = get_order_type(1))
      
      
    })  # table_description_event
    
    
    observeEvent(input$new_table, {
      
      values$new_table_ids <- get_new_table_ids(values$table_ids, base_url)
      
      if (is.null(values$new_table_ids)) {
        shinyalert("Error", "Error downloading list of tables" , type = "error")
      } else {
        showModal(select_new_table_dialog(names(values$new_table_ids),
                                          names(values$table_ids)))
      } 
    })
    
    observeEvent(input$new_table_ok, {
      
      new_table_desc <- input$new_table_desc
      if (new_table_desc == "") {
        return()
      }
      new_table_id <- values$new_table_ids[new_table_desc]
      if (is.na(new_table_id)) {
        warning(paste("Internal error ... Table", new_table_desc, 
                      "not in list of new tables"))
        return(invisible(NULL))
      }
      
      # additional check
      if (new_table_id %in% values$table_ids) {
        warning(paste("Internal error ... Table", new_table_desc, 
                      "already in list of tables"))
        return(invisible(NULL))
      }
      
      # add new table
      tryCatch({
        
        values$new_table <- create_new_table(new_table_id, base_url)
        values$new_table_id <- new_table_id
        values$new_table_desc <- new_table_desc
        
        base_table_desc <-  input$new_table_base_desc
        if (base_table_desc != "") {
          base_table_id <- values$table_ids[base_table_desc]
          base_table <- values$tables[[base_table_id]]
          ret <- call_update_table(values$new_table, base_table)
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
      
      values$tables[[values$new_table_id]]  <- values$new_table
      values$table_ids[values$new_table_desc] <- values$new_table_id
      
      # reorder the tables alphabetically
      ord <- order(values$table_ids)
      values$tables <- values$tables[ord]
      values$table_ids <- values$table_ids[ord]
      
      updateSelectInput(session, inputId = "table_desc",
                        choices = create_table_choices(names(values$table_ids)), 
                        selected = values$new_table_desc)
    }
    
    observeEvent(input$filled_table_ok, {
      insert_new_table()
      removeModal()
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
      delete_table_id <- values$table_ids[delete_table_desc]
      if (is.na(delete_table_id)) {
        warning(paste("Internal error ... Table", new_table_desc, 
                      "not in list of new tables"))
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
        output$table_pane <- renderUI({return(NULL)})
        values$table_id <- NA_character_
        values$table_desc <- NA_character_
      } else {
        updateSelectInput(session, inputId = "table_desc", choices = choices,
                          selected = values$table_desc)
      }
      
      
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
      id <- values$table_id
      if (!is.na(id)) {
        new_table <- create_new_table(id, base_url)
        ret <- call_update_table(new_table, values$tables[[id]])
        values$new_table <- ret$new_table
        if (length(ret$warnings) > 0) {
          showWarningsDialog(ret$warnings, "update_table_ok")
        } else {
          insert_updated_table()
          removeModal()
        }
      }
    })
    
    insert_updated_table <- function() {
      
      if (debug) cat("in insert_update_table\n")
      
      values$tables[[values$table_id]] <- values$new_table
      open_table(values, input, output, selected_tab = input$tabsetpanel, 
                 debug = debug)
    }
    
    observeEvent(input$update_table_ok, {
      insert_updated_table()
      removeModal()
    })
    
    observeEvent(input$tabsetpanel, {
      
      # a new tab has been selected
    
      if (debug) cat(sprintf("tab selection changed, new selected tab =  %s.\n", 
                         input$tabsetpanel))
      
      # first clean all tabs, the table for the selected tab will be recreated.
      make_empty_table <- function(name) {
        hot_id <- get_hot_id(values$table_id, name)
        output[[hot_id]] <- NULL
        return()
      }
      lapply(values$names, FUN = make_empty_table)
    
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
      # If current_type == SELECTED_ORDER, then the tables have already been 
      # re-rendered by function reorder_table() (see code above).
      if (current_type != SELECTED_FIRST_ORDER) {
        tab <- values$tables[[values$table_id]]$codes[[name]]
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
        tab <- values$tables[[values$table_id]]$codes[[name]]
        tab <- order_code_rows(tab, cbs_order = new_type == CBS_ORDER)
        values$tables[[values$table_id]]$codes[[name]] <- tab
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
      values$tables[[values$table_id]]$cbs_key_order[[name]] <- 
                                             input$order_table == CBS_ORDER
      
      if (debug) {
        cat("Updating cbs_key_order, new value = \n")
        print(values$tables[[values$table_id]]$cbs_key_order)
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
      values$tables[[values$table_id]]$order <- input$order_input_order
      
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