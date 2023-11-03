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
#' @importFrom shinybusy remove_modal_progress show_modal_spinner 
#' remove_modal_spinner
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE, browser,
                         debug = FALSE, base_url = NULL) {

  app <- create_shiny_app(ts_code_file, use_browser = use_browser,
                          browser = browser, debug = debug,
                          base_url = base_url)
  if (use_browser) {
    old_browser <- options("browser")
    tryCatch({
      options(browser = find_browser(browser))
      runApp(app, launch.browser = TRUE)
    }, finally = {
      options(browser = old_browser$browser)
    })
  } else {
    runApp(app)
  }
}

create_shiny_app <- function(ts_code_file, use_browser = TRUE, browser,
                         debug = FALSE, base_url = NULL,
                         testServer = FALSE) {
  
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
  
  CBS_ORDER <- "Original CBS Order"
  SELECTED_FIRST_ORDER <- "Selected First"
  
  button_width <- "200px"

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
    headerPanel("CBS Timeseries Coding"),
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
      selectInput("table_desc",
        label = NULL,
        choices = create_table_choices(table_descs)
      ),
      p(),
      newTableInput(id = "new_table", button_width = button_width),
      p(),
      deleteTableInput(id = "delete_table", button_width = button_width),
      p(),
      h3("Order Code Table"),
      "Select an order type below",
      "Press reorder to reorder after changing the table",
      p(),
      fluidRow(
        column(5,
          selectInput("table_order",
            label = NULL,
            choices = c(CBS_ORDER, SELECTED_FIRST_ORDER),
            width = button_width
          )
        ),
        column(1,
          actionButton("reorder", "Reorder",
            style = sprintf("width: %s", button_width)
          ),
          offset = 1
        )
      ),
      p(),
      h3("Update Table(s)"),
      "Updated Keys and Titles with recent information on the CBS website",
      p(),
      fluidRow(
        column(5, updateTableInput(
          id = "update_table",
          button_width = button_width
        )),
        column(1, updateAllTablesInput(
          id = "update_all_tables",
          button_width = button_width
        ), offset = 1),
      ),
      h3("Save Code"),
      paste("Save the Code to File", ts_code_file),
      p(),
      actionButton("save", "Save Codes",
        style = sprintf("width: %s", button_width)
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanelBody("empty", NULL),
        tabPanelBody(
          "tables",
          htmlOutput("table_title"),
          orderInput(
            inputId = "dimension_order",
            label = paste(
              "Order of dimensions used to create",
              "names (drag and drop items to",
              "change order):"
            ),
            items = "dummy"
          ),
          p(),
          icon("search"),
          tags$input(
            type = "text", id = "search_field",
            placeholder = "Search ..."
          ),
          tags$button(icon("caret-left"), id = "prev_button"),
          tags$button(icon("caret-right"), id = "next_button"),
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
                             table_order = CBS_ORDER,
                             selections = NULL)
    
    # disable the reorder button
    shinyjs::disable("reorder")
    
    tblcod_upd <- updateTableServer("update_table",
     table_open = reactive(values$table_open),
     tblcod = reactive(values$ts_code[[values$table_id]]),
     table_id = reactive(values$table_id),
     base_url = base_url,
     debug = debug
   )

    tscod_upd <- updateAllTablesServer("update_all_tables",
     table_present = reactive(values$table_present),
     tscod = reactive(values$ts_code),
     base_url = base_url,
     debug = debug
    )
    
    tblcod_new <- newTableServer("new_table",
      table_descs = reactive(values$table_descs),
      tscod = reactive(values$ts_code),
      base_url = base_url,
      debug = debug
    )
    
    delete_table_id <- deleteTableServer("delete_table",
      table_present = reactive(values$table_present),
      table_descs = reactive(values$table_descs),
      debug = debug
    )

    ############################################################################
    # internal function in the server function
    ##################s##########################################################
    
    render_hot_table <- function() {
      tab_data <- values$ts_code[[values$table_id]]$codes[[values$dimension]]
      selection <- values$selections[[values$table_id]][[values$dimension]]
      output$hot <- renderCodetable(codetable(tab_data,
        table_id = values$table_id,
        dimension = values$dimension,
        selection = selection
      ))
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
    
    default_selection <- list(row = 0, column = 0, row2 = 0, 
                              column2 = 0)
    
    # Open a table. This function is called when a new table has been selected
    # or when the selected table is updated with the update or update_all_tables
    # button.
    open_table <- function(selected_dimension) {
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
      
      if (missing(selected_dimension) || is.null(selected_dimension) || 
          is.na(selected_dimension) || !selected_dimension %in% dimensions) {
        dimension <- dimensions[1]
      } else {
        dimension <- selected_dimension
      }     
      
      values$dimension <- dimension
      
      # selections
      if (is.null(values$selections[[values$table_id]])) {
        selections <- sapply(dimensions, FUN = \(x) default_selection,
                             simplify = FALSE)
        values$selections[[values$table_id]] <- selections
      }
    
      # create a tabsetpanel with empty panels
      tabset_panel <- do.call(tabsetPanel, c(
        list(id = "dimension"),
        lapply(dimensions, FUN = tabPanel)
      ))
      output$dimension_tabsetpanel <- renderUI(tabset_panel)
      
      render_hot_table()   
      
      if (dimension != dimensions[1]) {
        updateTabsetPanel(session = session, inputId = "dimension",
                          selected = dimension)
      }
      if (testServer) session$setInputs(dimension = dimension)
      
      values$table_order <- get_order_type(dimension)
      updateSelectInput(session, "table_order", 
                        selected = values$table_order)
      if (testServer) session$setInputs(table_order = values$table_order)
      
      if (debug) cat("The table has been opened\n\n")
      
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
      # use a modal spinner to prevent any user input during reordering
      show_modal_spinner(text = "Reordering table")
      on.exit(remove_modal_spinner())
      cbs_order <- values$table_order == CBS_ORDER
      if (order_ts_code(cbs_order)) {
        # after reordering, restore the default selection
        values$selections[[values$table_id]][[values$dimension]] <- 
          default_selection
        render_hot_table()
        if (debug) cat("The table has been reordered\n\n")
      } else {
        if (debug) cat("The table was already in correct ordering.\n\n")
      }
      
      return(invisible())
    }
    
    ############################################################################
    # Observers 
    ############################################################################
    
    observeEvent(values$table_open, {
      if (debug) cat("\ntable_open modified. New value:", values$table_open, 
                     "\n")
      # Update different action buttons
      if (values$table_open) {
        shinyjs::enable("table_order")
      } else {
        shinyjs::disable("table_order")
      }
      if (values$table_open && values$table_order == SELECTED_FIRST_ORDER) {
        shinyjs::enable("reorder")
      } else {
        shinyjs::disable("reorder")
      }
      
      # make the panel with information of the table visible
      selected <- if (values$table_open) "tables" else "empty"
      updateTabsetPanel(inputId = "switcher", selected = selected)
      
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
          if (testServer) session$setInputs(table_desc = values$table_desc)
          return()
        }
        
        # Reorder data, this is only necessary for SELECTED_FIRST_ORDER
        # (for CBS_ORDER the table is already in the correct order).
        if (input$table_order == SELECTED_FIRST_ORDER &&
            order_ts_code(FALSE)) {
            # reset current selections
          values$selections[[values$table_id]][[values$dimension]] <- 
              default_selection
        } else {
          values$selections[[values$table_id]][[values$dimension]] <- 
            input$hot_selection
        }
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
      
      # save selection of the current table
      if (!is.na(values$dimension)) {
        values$selections[[values$table_id]][[values$dimension]] <- 
          input$hot_selection
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
      # Modified: do not reorder after a dimension change.
      #if (input$table_order == SELECTED_FIRST_ORDER) order_ts_code(FALSE)
      
      # now open table for a new dimension
      dimension <- input$dimension
      values$dimension <- dimension
      render_hot_table()
      values$table_order <- get_order_type(dimension)
      updateSelectInput(session, "table_order", selected = values$table_order)
      if (testServer) session$setInputs(table_order = values$table_order)
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
    
    observeEvent(input$dimension_order, {
      if (debug) {
        cat(sprintf("\nDimension order has changed, new value: %s\n\n",
                    paste(input$dimension_order, collapse = ", ")))     
      }
      if (!identical(sort(input$dimension_order), 
                     sort(values$ts_code[[values$table_id]]$order))) {
        shinyalert("Error", "Internal Error: dimension_order not correct")
        return()
      }
      values$ts_code[[values$table_id]]$order <- input$dimension_order
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
      hot_input <- input$hot
      table_id <- values$table_id
      dim <- values$dimension
      if (table_id !=  hot_input$table_id || dim != hot_input$dim) {
        shinyalert("Error", 
                   "Internal Error: table_id and dimension in hot table incorrect")
        return()
      }
      hot_data <- convert_hot_input_data(hot_input$data)
      if (is.null(hot_data)) {
        shinyalert("Error", "Internal error: data in hot table not correct")
        return()
      }
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
    
    # observeEvent(input$hot_selection, {
    #     cat("selection changed:\n")
    #     print(input$hot_selection)
    # })
    # 
    ############################################################################
    # Observers for adding a new table or deleting a table
    ############################################################################
   
    observeEvent(tblcod_new(), {
      tblcod_new <- tblcod_new()
      table_id_new <- tblcod_new$id
      if (debug) {
        cat(sprintf("\nA new table has been selected, new table = %s\n",
                    table_id_new))
        #print(tscod_new)
      }
      
      table_ids_new <- c(values$table_ids, table_id_new)
      table_desc_new <- get_table_description(table_id_new,
        tblcod_new$short_title)
      
      table_descs_new <- values$table_descs
      table_descs_new[table_id_new] <- table_desc_new
      
      values$ts_code[[table_id_new]]  <- tblcod_new
      
      # sort the table ids alphabetically: 
      table_ids_new <- sort(table_ids_new)
      table_descs_new <- table_descs_new[table_ids_new]
      # use new_ts_code, because otherwise the attributes (class and 
      # package version) are lost.
      values$ts_code <- new_ts_code(values$ts_code[table_ids_new])
      values$table_ids <- table_ids_new
      values$table_descs <- table_descs_new
      values$table_present <- TRUE
      updateSelectInput(session,
        inputId = "table_desc",
        choices = create_table_choices(values$table_descs),
        selected = table_desc_new
      )
      if (testServer) {
        session$setInputs(table_desc = table_desc_new)
      }
    })
   
    observeEvent(delete_table_id(), {
      if (debug) cat(sprintf("\nDeleting table %s\n\n", delete_table_id()))
      
      delete_id <- delete_table_id()
      values$ts_code[[delete_id]] <- NULL
      
      # update table_ids
      idx <- match(delete_id, values$table_ids)
      values$table_ids <- values$table_ids[-idx]
      values$table_descs <- values$table_descs[values$table_ids]
      
      if (values$table_open && delete_id == values$table_id) {
        values$table_id <- NA_character_
        values$table_desc <- NA_character_
        values$dimension <- NA_character_
        values$table_open <- FALSE
        values$table_order <- CBS_ORDER
        values$selections[[delete_id]] <- NULL
      }

      # update table inputs
      selected <- if (values$table_open) values$table_desc else NULL
      updateSelectInput(session, inputId = "table_desc", 
                        choices = create_table_choices(values$table_descs),
                        selected = selected)
    
      values$table_present <- length(values$ts_code) > 0
    })
    
    ############################################################################
    # Observers for updating the selected table or all tables.
    ############################################################################
    
    observeEvent(tblcod_upd(), {
      table_code_upd <- tblcod_upd()
      table_id <- table_code_upd$id
      if (debug) {
        cat(sprintf("\nUpdate voor tabel %s\n\n", table_id))
        #print(table_code_upd)
      }
      
      if (table_id != values$table_id) {
        shinyalert("Error", 
                   "Internal Error: table_id of update does not agree with id")
        return()
      }

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
        if (testServer) session$setInputs(table_desc = new_table_desc)
      }
      
      # now open the table
      open_table(selected_dimension = values$dimension)
      
      if (debug) cat("\n")
      
      return(invisible())
    })
    
    observeEvent(tscod_upd(), {
      if (debug) {
        cat("\nUpdate for all tables\n\n")
      }
      
      ts_code_upd <- tscod_upd()
      
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
        updateSelectInput(session,
          inputId = "table_desc",
          choices = create_table_choices(values$table_descs),
          selected = selected
        )
        if (testServer) session$setInputs(table_desc = selected)
      }
      
      if (open_table_modified) {
        if (debug) cat("Data for table", values$table_id, " updated.\n")
        open_table(selected_dimension = values$dimension)
      }
      
      if (debug) cat("\n")
      
      return(invisible())
    })
  }
  
  return(shinyApp(ui = ui, server = server))
}
