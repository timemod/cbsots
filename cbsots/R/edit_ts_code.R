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
#'  (only for developers)
#' @param base_url optionally specify a different server. Useful for third party
#' data services implementing the same protocol.
#' @import shiny
#' @import shinyjqui
#' @importFrom utils packageVersion
#' @importFrom utils packageName
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_progress
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE, browser,
                         debug = FALSE, base_url = NULL) {
  
  if (file.exists(ts_code_file)) {
    
    ts_code <- readRDS(ts_code_file)
    ts_code <- convert_ts_code(ts_code)
    
    # Reorder all table_code objects in ts_code, to make sure that
    # all tables are correctly ordered.
    ts_code[] <- lapply(ts_code, FUN = order_table_code)
    
    if (is.null(ts_code)) {
      stop(paste("File", ts_code_file, "does not contain a ts_code object"))
    }
    
  } else {
    
    ts_code <- new_ts_code()
    
  }
  
  CBS_ORDER <- "Original CBS order"
  SELECTED_FIRST_ORDER <- "Selected first"
  
  # create a vector with table ids and a named vector with table descriptions
  if (length(ts_code) > 0) {
    table_ids <- names(ts_code)
    table_descs <- get_table_descs(ts_code)
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
        column(5, updateTableInput(id = "update_table")),
        column(1, updateAllTablesInput(id = "update_all_table"), offset = 1),
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
          icon("search"),
          tags$input(type = "text", id = "search_field",
                     placeholder = "Search ..."),
          tags$button(icon("caret-left"), id = "prev_button"),
          tags$button(icon("caret-right"),  id = "next_button"),
          p(),
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
                             dimension = NA_character_,
                             table_order = CBS_ORDER)
    
    # disable some input elements
    input_ids <- c("update_table", "table_order", "reorder")
    if (!table_present) {
      input_ids <- c(input_ids, "update_all_tables", "delete_table")
    }
    invisible(lapply(input_ids, FUN = shinyjs::disable))
  
    
    tblcod_for_upd <- reactive({
      cat("\nEvaluating ts_code_for_upd\n\n")
      tblcod <- values$ts_code[[values$table_id]]
      tblcod$order <- input$dimension_order
      tblcod
    })
    
    tblcod_upd <- updateTableServer("update_table",
     table_open = reactive(values$table_open),
     tblcod = tblcod_for_upd,
     table_id = reactive(values$table_id),
     base_url = base_url,
     debug = debug
   )
    
  
    
    ############################################################################
    # internal function in the server function
    ##################s##########################################################
    
    render_hot_table <- function() {
      tab_data <- values$ts_code[[values$table_id]]$codes[[values$dimension]]
      output$hot <- renderCodetable(codetable(tab_data))
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
    
    # Open a table. This function is called when a new table has been selected
    # or when the selected table is updated with the update or update_all_tables
    # button.
    open_table <- function() {
      if (debug) {
        cat(sprintf("\nOpening table_id %s (function open_table()).\n", 
                    values$table_id))
      }
      
      output$table_title <- renderUI(HTML("<h3>Tabel", values$table_desc, 
                                          "</h3>"))
      
      updateOrderInput(
        session = getDefaultReactiveDomain(),
        inputId = "dimension_order",
        items = values$ts_code[[values$table_id]]$order
      )
      
      dimensions <- get_dimensions(values$ts_code[[values$table_id]])
      dimension <- dimensions[1]
      
      values$dimension <- dimension
      
      # create a tabsetpanel with empty panels
      tabset_panel <- do.call(tabsetPanel, c(
        list(id = "dimension"),
        lapply(dimensions, FUN = tabPanel)
      ))
      output$dimension_tabsetpanel <- renderUI(tabset_panel)
      
      render_hot_table()   
      
      values$table_order <- get_order_type(dimension)
      updateSelectInput(session, "table_order", 
                        selected = values$table_order)
      
      if (debug) cat("The table has been opened\n\n")
      
      return(invisible())
    }
    
     # save dimension ordering:
    fetch_dimension_order <- function() {
      if (!identical(sort(input$dimension_order), 
                     sort(values$ts_code[[values$table_id]]$order))) {
        shinyalert("Error", "Internal Error: dimension_order not correct")
        return()
      }
      values$ts_code[[values$table_id]]$order <- input$dimension_order
      return(invisible())
    }
    
    # Order the ts code for table value$table_id and dimension value$dimension
    # Returns TRUE is the data has actually been reordered.
    order_ts_code <- function(cbs_order) {
      table_id <- values$table_id
      dim <- values$dimension
      code <- values$ts_code[[table_id]]$codes[[dim]]
      code_ordered <- order_code_rows(code, cbs_order = cbs_order)
      values$ts_code[[table_id]]$codes[[dim]] <- code_ordered
      return(!identical(code$Key, code_ordered$Key))
    }
    
    # Reorder the table that is currently displayed.
    reorder_table <- function() {
      if (debug) cat("\nIn reorder_table\n")
      cbs_order <- values$table_order == CBS_ORDER
      if (order_ts_code(cbs_order)) {
        render_hot_table()
        if (debug) cat("The table has been reordered\n\n")
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
      # use new_ts_code, because otherwise the attributes (class and 
      # package version) are lost.
      values$ts_code <- new_ts_code(values$ts_code[ord])
      values$table_ids <- values$table_ids[ord]
      values$table_descs <- values$table_descs[values$table_ids]
      values$table_present <- TRUE
      
      updateSelectInput(session, inputId = "table_desc",
                        choices = create_table_choices(values$table_descs),
                        selected = values$new_table_desc
      )
    }
    
    # update all tables
    update_all_tables <- function(ts_code_upd) {
      if (debug) cat("\nUpdating all tables\n")
  
      # First check if data in the open table has been modified
      if (values$table_open) {
        table_code_old <- values$ts_code[[values$table_id]]
        table_code_new <- ts_code_upd[[values$table_id]]
        open_table_modified <- !identical(table_code_old, table_code_new)
      } else {
        open_table_modified <- FALSE
      }
      
      # Update ts_code for all tables
      values$ts_code <- ts_code_upd
      
      # Check if table_descs have been modified
      table_descs_new <- get_table_descs(ts_code_upd)
      if (!identical(values$table_descs, table_descs_new)) {
        if (debug) cat("Table descriptions have changed.\n")
        values$table_descs <- table_descs_new
        if (values$table_open) {
          selected <- values$table_desc
          # The following statement is essential: if prevents that 
          # action is taken in the observer for input$table_desc:
          values$table_desc <- table_descs_new[values$table_id]
        } else {
         selected <- NULL
        }
        updateSelectInput(session, inputId = "table_desc",
                          choices = create_table_choices(values$table_descs), 
                          selected = selected)
      }
      
      if (open_table_modified) {
        if (debug) cat("Data for table", values$table_id, " updated.\n")
        open_table()
      }
      
      if (debug) cat("\n")
      
      return(invisible())
    }
    
    ############################################################################
    # Observers 
    ############################################################################
    
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
    
    observeEvent(input$table_desc, {
      if (debug) {
        cat("\nTable desc has changed:\n")
        cat("input$table_desc:", input$table_desc, "\n")
        cat("values$table_desc:", values$table_desc, "\n")
      }
      if (input$table_desc == "") {
        cat("\nNo table selected, no action required\n")
        return()
      }
      if (values$table_open && input$table_desc == values$table_desc) {
        # This situation occurs when the table has been updated and the 
        # short table title (and hence table_desc) has been modified.
        # Do not do do anything in this case.
        if (debug) {
          cat("No action required because",
              "input$table_desc == values$table_desc\n\n")
        }
        return()
      }
      
      if (debug) {
        cat(sprintf("\nA table has been selected, table_desc = %s\n", 
                    input$table_desc))
      }

      if (values$table_open) {
        
        if (check_duplicates(values$ts_code, values$table_id, 
                             values$dimension)) {
          # The user must first correct the duplicates, therefore do not 
          # open the new table.
          updateSelectInput(session, inputId = "table_desc",
                            selected = values$table_desc)
          return()
        }
        
        # Reorder data, this is only necessary for SELECTED_FIRST_ORDER
        # (for CBS_ORDER the table is already in the correct order).
        if (input$table_order == SELECTED_FIRST_ORDER) order_ts_code(FALSE)
        
        fetch_dimension_order()
      }
      
      new_table_id <- get_table_id(input$table_desc)
      if (debug) {
        cat(sprintf("\nOpening table with id = %s\n\n", new_table_id))
      }
      
      values$table_id <- new_table_id
      values$table_desc <- values$table_descs[new_table_id]
      values$table_open <- TRUE
   
      open_table()
  
    }, ignoreInit = TRUE)  # table_description_event
    
    observeEvent(input$dimension, {
      if (values$dimension == input$dimension) return()
      if (debug) {
        cat(sprintf("\nSelected dimension changed for table %s:\n", 
                    values$table_id))
        cat("input$dimension:", input$dimension, "\n")
        cat("values$dimension:", values$dimension, "\n\n")
      }
      
      if (check_duplicates(values$ts_code, values$table_id, 
                           values$dimension)) {
        # There are duplicates in the code of the current table, which 
        # the user must correct first. Select original tab and return
        updateTabsetPanel(session, "dimension", selected = values$dimension)
        return()
      }
      
      # Reorder for the table that is currently open, this is only necessary 
      # for SELECTED_FIRST_ORDER (for CBS_ORDER the table is already in the 
      # correct order).
      if (input$table_order == SELECTED_FIRST_ORDER) order_ts_code(FALSE)
      
      # now open table for a new dimension
      dimension <- input$dimension
      values$dimension <- dimension
      render_hot_table()
      values$table_order <- get_order_type(dimension)
      updateSelectInput(session, "table_order", selected = values$table_order)
    })
    
    observeEvent(input$table_order, {
      if (input$table_order == values$table_order) return()
      if (debug) {
        cat(sprintf("\nThe table order has been modified (dimension = %s).\n", 
                    values$dimension))
      }

      values$table_order <- input$table_order 
      
      reorder_table()
      
      # update cbs_key_order
      values$ts_code[[values$table_id]]$cbs_key_order[[values$dimension]] <-
        input$table_order == CBS_ORDER
      
      if (debug) {
        cat("Updating cbs_key_order, new value = \n")
        print(values$ts_code[[values$table_id]]$cbs_key_order)
        cat("\n\n")
      }
      
      # update reorder button
      if (values$table_open && input$table_order == SELECTED_FIRST_ORDER) {
        shinyjs::enable("reorder")
      } else {
        shinyjs::disable("reorder")
      }      
    }, ignoreInit = TRUE)
    
    observeEvent(input$reorder, {
      if (debug) cat(sprintf("\nReorder button pressed (dimension = %s).\n", 
                             values$dimension))
      reorder_table()
    })
    
    observeEvent(input$save, {
      if (debug) cat("\nSave button pressed\n")
      
      if (values$table_open) {
        if (input$table_order == SELECTED_FIRST_ORDER) reorder_table()

        if (check_duplicates(values$ts_code, values$table_id, 
                             values$dimension)) return()
        
        fetch_dimension_order()
      }
    
      saveRDS(values$ts_code, file = ts_code_file)
      
      if (debug) {
        cat("ts_code:\n")
        print(values$ts_code)
        cat("\nsaved to ", ts_code_file, "\n\n")
      }
    })
    
    observeEvent(input$hot, {
      if (debug) cat("\nHot table data changed\n")
      hot_data <- input$hot
      if (is.null(hot_data)) {
        shinyalert("Error", "Internal error: hot data not available")
        return()
      }
      hot_data <- convert_codetable(input$hot)
      if (is.null(hot_data)) {
        shinyalert("Error", "Internal error: hot data not correct")
        return()
      }
      
      table_id <- values$table_id
      dim <- values$dimension
      # TODO: store table_id and dimension is codetable widget and retrieve 
      # them
      
      data_old <- values$ts_code[[table_id]]$codes[[dim]][, 1:4]
      if (debug) {
        cat("table_id = ", table_id, "\n")
        cat("dimension = ", dim, "\n")
        cat("old values:\n")
        print(head(data_old[, 1:3]))
        cat("\nNew values:\n")
        print(head(hot_data[, 1:3]))
      }
      
      # check hot data
      if (!identical(data_old$Key, hot_data$Key) ||
          !identical(data_old$Title, hot_data$Title)) {
        shinyalert("Error", "Internal Error: hot data not correct")
        return()
      }
      
      # Store hot data in values$ts_code
      values$ts_code[[table_id]]$codes[[dim]][, 1:4] <- hot_data[, 1:4]
      
      if (debug) cat("ts_code has been updated with hot data\n\n")
    })
    
    ############################################################################
    # Observers for adding a new table or deleting a table
    ############################################################################
    
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
        
        values$new_table <- table_code(new_table_id, base_url)
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
      if (debug) cat("\nIn delete_table_confirmed\n")
      
      values$ts_code[[values$delete_table_id]] <- NULL
      
      # update table_ids
      idx <- pmatch(values$delete_table_id, values$table_ids)
      values$table_ids <- values$table_ids[-idx]
      values$table_descs <- values$table_descs[values$table_ids]
      
      if (values$table_open && values$delete_table_id == values$table_id) {
        values$table_id <- NA_character_
        values$table_desc <- NA_character_
        values$dimension <- NA_character_
        values$table_open <- FALSE
        values$table_order <- CBS_ORDER
      }

      # update table  inputs
      selected <- if (values$table_open) values$table_desc else NULL
      updateSelectInput(session, inputId = "table_desc", 
                        choices = create_table_choices(values$table_descs),
                        selected = selected)
    
      values$table_present <- length(values$ts_code) > 0
      removeModal()
    })
    
    ############################################################################
    # Observers for updating the selected table or all tables.
    ############################################################################
    
    observeEvent(tblcod_upd(), {
      table_code_upd <- tblcod_upd()
      table_id <- table_code_upd$id
      if (debug) {
        cat(sprintf("\nUpdate voor tabel %s\n", table_id, "\n\n"))
        #print(table_code_upd)
      }

      # TODO: check table_id == values$table_id?
      
      # Save the original short title
      short_title_old <- values$ts_code[[table_id]]$short_title
      
      # Update ts_code
      values$ts_code[[table_id]] <- table_code_upd
      
      # Handle change of short table title
      short_title_new <-  table_code_upd$short_title
      if (short_title_old != short_title_new) {
        new_table_desc <- get_table_description(values$table_id, 
                                                short_title_new)
        if (debug) cat("Table description has changed: ", new_table_desc, "\n")
        values$table_descs[values$table_id] <- new_table_desc
        # The following statement is essential: if prevents that 
        # action is taken in the observer for input$table_desc:
        values$table_desc <- new_table_desc
        updateSelectInput(session, inputId = "table_desc",
                          choices = create_table_choices(values$table_descs), 
                          selected = new_table_desc)
      }
      
      # now open the table
      open_table()
      
      if (debug) cat("\n")
      return(invisible())
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
      if (debug) cat("\nUpdate all tables confirmed\n")
      removeModal()
      if (values$table_open) fetch_dimension_order()
      retval <- perform_update_all_tables(values$ts_code, base_url = base_url,
                                          debug = debug)
      ts_code_upd <- retval$ts_code_upd
      if (identical(ts_code_upd, values$ts_code)) {
        if (debug) cat("All tables are already up to date, nothing to do")
        return()
      }
      warning_ids <- retval$warning_ids
      if (length(warning_ids) > 0) {
        values$ts_code_upd <- ts_code_upd
        wmsg <- paste("For tables", paste(warning_ids, collapse = ", "),
                      "some old keys do not match perfectly with new keys.\n",
                      "Check the match reports in directory 'match_reports'.")
        wmsg <- strwrap(wmsg, width = 80)
        wmsg <- paste(wmsg, collapse = "\n")
        showWarningsDialog(wmsg, "update_all_tables_ok")
      } else {
        update_all_tables(ts_code_upd)
      }
      return()
    })
    
    observeEvent(input$update_all_tables_ok, {
      if (debug) cat("Update all tables ok\n")
      removeModal()
      update_all_tables(values$ts_code_upd)
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