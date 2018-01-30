#' Edit timeseries codes
#' 
#' @param ts_code_file a filename with timeseries codes
#' @param use_browser if \code{TRUE}, then display the graphical user interface
#'  in the browser. Otherwise the RStudio viewer is used.
#' @import shiny
#' @import rhandsontable
#' @import shinyjqui
#' @import cbsodataR
#' @importFrom utils packageVersion
#' @export
edit_ts_code <- function(ts_code_file, use_browser = TRUE) {
  
  # TODO: special read function, check the package version
  table_code_collection <- readRDS(ts_code_file)
  tables <- table_code_collection$table_code
  
  debug <- FALSE
 
  if (packageVersion("rhandsontable") == "0.3.5") {
    # version rhandsontable 0.3.5 does not work correctly in  RStudio viewer and
    # the internet explorer. This problem has been fixed in the current development
    # version.
    stop("rhandsontable 0.3.5 does not work correclty. Please install the newest",
         "version with command\n devtools::install_github(\"jrowen/rhandsontable\")")
  }
  
  # order tables alphabetically
  tables <- tables[order(names(tables))]
  
  ui <- pageWithSidebar(
    
    headerPanel('Timeseries coding'),
    sidebarPanel(
      uiOutput("table_chooser"),
      h2("Open a new table"),
      actionButton("new_table", "New table"),
      p(),
      h2(paste("Save all code to file", ts_code_file)),
      actionButton("save", "Save codes")
    ),
    mainPanel(
      uiOutput('tabel')
    )
  )
  
  server <- function(input, output, session) {
    
    session$onSessionEnded(shiny::stopApp)
    
    values <- reactiveValues(tables = tables,
                             table_id = names(tables)[1],
                             orderInput_count = 0, 
                             last_modified = Sys.time())
    
    short_titles <- sapply(tables, FUN = function(x) return(x$short_title))
    
    table_descriptions <- paste(names(tables), "-", short_titles)
    
    
    #
    # conversion tables between table_descriptions <> table_ids
    #
    table_ids_dict <- names(tables)
    names(table_ids_dict) <- table_descriptions
    
    table_description_dict <- table_descriptions
    names(table_description_dict) <- names(tables)
    
    values$table_descriptions <- paste(names(tables), "-", short_titles)
    values$table_ids_dict <- table_ids_dict
    values$table_description_dict <- table_description_dict
    
    
    output$table_chooser <- renderUI(
      selectInput("table_description", 
                  label = "Choose a table for editing",
                  choices = values$table_descriptions,
                  selected = values$table_descriptions[1])
    )
    
    observeEvent(input$table_description, {
      values$table_id <- values$table_ids_dict[input$table_description]
    })
    
    dataModal <- function(failed = FALSE) {
      table_info <- get_table_list(select = c("Identifier", "ShortTitle"))
      new_tables <- setdiff(table_info$Identifier, names(isolate(values$tables)))
      table_info <- table_info[table_info$Identifier %in% new_tables, ]
      table_info <- table_info[order(table_info$Identifier), ]
      new_table_descriptions <- paste(table_info$Identifier, "-", 
                                      table_info$ShortTitle)
      
      new_table_id_dict <-   table_info$Identifier
      names(new_table_id_dict) <- new_table_descriptions
      values$new_table_id_dict <- new_table_id_dict
   
      
      modalDialog(
        selectInput("new_table_description", 
                  label = "Choose a table",
                  choices = new_table_descriptions,
                  selected = new_table_descriptions[1],
                  width = "200%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("new_table_ok", "OK")
        )
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$new_table, {
      showModal(dataModal())
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$new_table_ok, {
      # Check that data object exists and is data frame.
      new_table_id <- values$new_table_id_dict[input$new_table_description]
      new_table_id <- as.character(new_table_id)
      
      values$tables[[new_table_id]] <- create_new_table(new_table_id)
      values$tables <- values$tables[sort(names(values$tables))]
      
      short_titles <- sapply(values$tables, FUN = function(x) return(x$short_title))
      values$table_descriptions <- paste(names(values$tables), "-", short_titles)
      
      #
      # conversion tables between table_descriptions <> table_ids
      #
      values$table_ids_dict <- names(values$tables)
      names(values$table_ids_dict) <- values$table_descriptions
      
      values$table_description_dict <- values$table_descriptions
      names(values$table_description_dict) <- names(values$tables)
  
      removeModal()
    })
    
    # 
    # prepare tabbed pane
    #
    
    
    make_panel <- function(name) {
      return(tabPanel(name, rHandsontableOutput(name)))
    }
  
    #
    # create tables
    #
    
    observeEvent(values$table_id, {
  
      # save old results
      if (!is.null(values$old_table_id) && values$old_table_id != input$table_id) {
        if (debug) {
          cat(paste("new table", input$table_id, "\n"))
          cat("Topic:\n")
          print(head(values$Topic[ , 1:2]))
        }
        values$tables[[values$old_table_id]]$last_modified <- values$last_modified
        values$tables[[values$old_table_id]]$codes$Topic[, 1:4] <- values$Topic
        dimensions <- names(values$tables[[values$old_table_id]]$codes)
        for (dimension in dimensions) {
          values$tables[[values$old_table_id]]$codes[[dimension]][ ,1:4] <- 
                                                         values[[dimension]]
          if (debug) {
            cat(paste("Dimension ", dimension, ":\n"))
            print(head(values[[dimension]][ , 1:2]))
          }
        }
      }
      
      # remove previous elements
      dimensions <- values$names[-1]
      for (dimension in dimensions) {
         values[[dimension]] <- NULL
      }
      
      # add new elements
      values$names <- names(values$tables[[values$table_id]]$codes)
      values$Topic <- values$tables[[values$table_id]]$codes$Topic[, 1:4]
      dimensions <- values$names[-1]
      for (dimension in dimensions) {
        values[[dimension]] <- values$tables[[values$table_id]]$codes[[dimension]][, 1:4]
      }
      values$last_modified <-  values$tables[[values$table_id]]$last_modifed
      
      make_table <- function(name) {
        # NOTES:
        # 1. It is neccesarry to set the height of the table, otherwise
        #    the vertical scroll bar does not appear
        output[[name]] <- renderRHandsontable({
          if (!is.null(values[[name]])) {
            rhandsontable(values[[name]], readOnly = TRUE, height = 500, 
                          overflow = "hidden") %>%
              hot_cols(fixedColumnsLeft = 3) %>%
              hot_col(col = c("Select", "Code"), readOnly = FALSE) %>%
              hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
          } else {
            rhandsontable(as.data.frame("dummy"))
          }
        })
      }
      lapply(values$names, FUN = make_table)
        
      # 
      # observers for the tables
      #
      make_observer <- function(name) {
        observeEvent(input[[name]], {
          if (debug) cat(paste("table", name , "changed\n"))
          if (!is.null(input[[name]])) {
            df_input  <- hot_to_r(input[[name]])
            df_values <- values[[name]]
            if (!is.null(df_values) && !is.null(df_input) &&
                # the next conditions are necessary because sometimes 
                # input[[name]] is not synchronous with values.
                identical(sort(df_input$Key), sort(df_values$Key)) &&
                identical(sort(df_input$Title), sort(df_values$Title))) {
              
              if (debug) {
                cat("current values\n")
                print(head(df_input[, 1:2]))
              }
              if (!identical(df_input$Select, df_values$Select)) {
                # selection has changed
                if (debug) cat("Selection has changed\n")
                orig_key_order <- values$tables[[values$table_id]]$codes[[name]]$OrigKeyOrder
                values[[name]] <- order_code_rows(df_input, orig_key_order)
              } else {
                values[[name]] <- df_input
              }
              values$last_modified <- Sys.time()
              if (debug) {
                cat("new values\n")
                print(head(values[[name]][, 1:2]))
              }
            }
          }
        })
        
        output$tabel <- renderUI({
          
          table_id <- values$table_id
          table_items <- names(values$tables[[table_id]]$code)
          myTabs <- lapply(table_items, make_panel)
          
          values[["orderInput_count"]] <- isolate(values[["orderInput_count"]]) + 1
          
          orderInputId <- paste0("order", isolate(values[["orderInput_count"]]))
          ret <- list(h2(paste("Tabel", values$table_description_dict[table_id])), br(),
                      list(p()), orderInput(inputId = orderInputId, 
                                            label = "Order for name generation", 
                                            items = values$tables[[table_id]]$order),
                      p(), h3("Codes"),
                      do.call(tabsetPanel, myTabs))
          
          observeEvent(input[[paste0(orderInputId, "_order")]], {
            if (debug) {
              cat("order has changed\n")
              print(input[[paste0(orderInputId, "_order")]])
            }
            values$tables[[table_id]]$order <- input[[paste0(orderInputId, "_order")]]
            values$last_modified <- Sys.time()
          })
          
          return(ret)
        })
        
      }
        
        
      lapply(values$names, make_observer)
        
      values$old_table_id <- input$table_id
      
    })   # observeEvent
      
    
    observeEvent(input$save, {
      values$tables[[values$table_id]]$last_modifed <- values$last_modified
      if (!is.null(input$Topic)) {
        values$tables[[values$table_id]]$codes$Topic[, 1:4] <- 
               hot_to_r(input[["Topic"]])
      }
      for (dimension in values$names[-1]) {
        if (!is.null(input[[dimension]])) {
           values$tables[[values$table_id]]$codes[[dimension]][, 1:4] <- 
                           hot_to_r(input[[dimension]])
        }
      }
      
      
      table_code_collection <-
        structure(list(package_version = packageVersion("cbsots"),
                       table_code = values$tables),
                  class = "table_code_collection")
    
      #print(table_code_collection)
      saveRDS(table_code_collection, file = ts_code_file)
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