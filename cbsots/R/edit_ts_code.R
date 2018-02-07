#' Edit timeseries codes
#' 
#' @param output_file the name of a file where the timeseries coding is stored.
#' The filename usually has extension \code{.rds}.
#' @param input_file a filename with timeseries codes. This file does not have
#' to exist yet. If specified, it should be an rds file 
#' containing a \code{table_code_collection} object.
#' @param use_browser if \code{TRUE}, then display the graphical user interface
#'  in the browser. Otherwise the RStudio viewer is used.
#' @import shiny
#' @import rhandsontable
#' @import shinyjqui
#' @import cbsodataR
#' @importFrom utils packageVersion
#' @export
edit_ts_code <- function(output_file, input_file, use_browser = TRUE) {
  
  if (!missing(input_file)) {
    if (!file.exists(input_file)) {
      stop(paste("File", input_file, "does not exist"))
    }
    
    # TODO: special read function, check the package version
    table_code_collection <- readRDS(input_file)
    
    if (!inherits(table_code_collection, "table_code_collection")) {
      stop(paste("File", input_file, "does not contain a",
                "table_code_collection object."))
    }
    
  } else {
    table_code_collection <-  
      structure(list(package_version = packageVersion("cbsots"),
                     table_code = list()),
                class = "table_code_collection")
  }
  
  tables <- table_code_collection$table_code
 
  debug <- FALSE
 
  ui <- pageWithSidebar(
    
    headerPanel('Timeseries coding'),
    sidebarPanel(
      uiOutput("table_chooser"),
      h2("Open a new table"),
      actionButton("new_table", "New table"),
      p(),
      h2("Delete a new table"),
      actionButton("delete_table", "Delete table"),
      p(),
      h2(paste("Save code to file", output_file)),
      actionButton("save", "Save codes")
    ),
    mainPanel(
      uiOutput('tabel')
    )
  )
  
  server <- function(input, output, session) {
    
    session$onSessionEnded(shiny::stopApp)
    
    values <- reactiveValues(tables = tables, table_id = names(tables)[1])
    
    if (length(tables) > 0) {
    
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
    } else {
      values$table_descriptions <- character(0)
      values$table_ids_dict <- character(0)
      values$table_description_dict <- character(0)
    }
    
    output$table_chooser <- renderUI(
      selectInput("table_description",
                  label = "Choose a table for editing",
                  choices = values$table_descriptions,
                  # the next expression can be isolated: the selectInput will
                  # be recreated when values$table_descriptions changes.
                  selected = values$table_description_dict[isolate(values$selected_table)])
    )
    
    observeEvent(input$table_description, {
      
      # check for duplicates, otherwise don't change
      if (!is.null(values$old_table_id)) {
        if (check_duplicates(session, values)) return()
      }
      
      values$table_id <- values$table_ids_dict[input$table_description]
    })
    
    newTableModal <- function(failed = FALSE) {
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
      showModal(newTableModal())
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$new_table_ok, {

      new_table_id <- values$new_table_id_dict[input$new_table_description]
      new_table_id <- as.character(new_table_id)
      
      values$tables[[new_table_id]] <- create_new_table(new_table_id)
      values$tables <- values$tables[sort(names(values$tables))]
      
      
      values$selected_table <- new_table_id
      
      
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
    
    
    deleteTableModal <- function(failed = FALSE) {
     
      modalDialog(
        selectInput("delete_table_description", 
                    label = "Choose a table",
                    choices =  isolate(values$table_descriptions),
                    selected = isolate(values$table_descriptions[1]),
                    width = "200%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("delete_table_ok", "OK")
        )
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$delete_table, {
      showModal(deleteTableModal())
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$delete_table_ok, {
      
      delete_table_desc <- input$delete_table_description
      delete_table_id <- values$table_ids_dict[delete_table_desc]
      delete_table_id <- as.character(delete_table_id)
      
      
      # TODO: get index
      cat("delete_table_id = \n")
      print(delete_table_id)

      values$tables[[delete_table_id]] <- NULL

      values$table_descriptions <- setdiff(values$table_descriptions, 
                                           delete_table_desc)
   
      cat("table_descriptions = \n")
      print(values$table_derscriptions)
      
      #
      # conversion tables between table_descriptions <> table_ids
      #
      values$table_ids_dict <- names(values$tables)
      names(values$table_ids_dict) <- values$table_descriptions
      
      values$table_description_dict <- values$table_descriptions
      names(values$table_description_dict) <- names(values$tables)
  
      if (delete_table_id == values$table_id) {
        # select the first table
        values$selected_table <- names(values$tables[1])
      }
      
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
      
      if (debug) {
        cat(sprintf("Table id changed, new value = %s\n", values$table_id))
      }
  
      # save old results
      if (!is.null(values$old_table_id) && 
          values$old_table_id != values$table_id) {
      
        update_tables(values$old_table_id, values, input, debug)
        
        # remove previous dimensions (not Topic)
        for (dimension in values$names[-1]) {
          values[[dimension]] <- NULL
        }
      }
      
      # copy tables
      values$names <- names(values$tables[[values$table_id]]$codes)
      dimensions <- values$names[-1]
      for (name in values$names) {
        values[[name]] <- values$tables[[values$table_id]]$codes[[name]][, 1:4]
      }
      
      make_table <- function(name) {
        # NOTES:
        # 1. It is neccesarry to set the height of the table, otherwise
        #    the vertical scroll bar does not appear
        output[[name]] <- renderRHandsontable({
          if (!is.null(values[[name]])) {
            rhandsontable(values[[name]], readOnly = TRUE, height = 500, 
                          overflow = "hidden", search = TRUE, 
                          renderAllRows = FALSE) %>%
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
                # only respond to changes in Select or Code
                identical(df_input$Key, df_values$Key) &&
                identical(df_input$Title, df_values$Title)) {
              
              if (debug) {
                cat("current values\n")
                print(head(df_input[, 1:3]))
              }
              if (!identical(df_input$Select, df_values$Select)) {
                # selection has changed
                if (debug) cat("Selection has changed\n")
                orig_key_order <- values$tables[[values$table_id]]$codes[[name]]$OrigKeyOrder
                values[[name]] <- order_code_rows(df_input, orig_key_order)
              } else {
                values[[name]] <- df_input
              }
              if (debug) {
                cat("new values\n")
                print(head(values[[name]][, 1:3]))
              }
            }
          }
        })
        
        output$tabel <- renderUI({
          
          table_id <- values$table_id
          table_items <- names(values$tables[[table_id]]$code)
          myTabs <- lapply(table_items, make_panel)
          
          ret <- list(h2(paste("Tabel", values$table_description_dict[table_id])), br(),
                      list(p()), 
                      orderInput(inputId = "order_input", 
                                 label = "Order for name generation", 
                                 items = values$tables[[table_id]]$order),
                      list(p()),
                      textInput(inputId = "searchField",  
                                label = "Search in tabel (enter a text followed by ENTER)"),
                      p(), h3("Codes"),
                      do.call(tabsetPanel, c(list(id = "selected_tab"), myTabs)))
          
          return(ret)
        })
        
      }
        
      lapply(values$names, make_observer)
        
      values$old_table_id <- values$table_id
      
    })   # observeEvent
      
    
    observeEvent(input$save, {
      
      if (check_duplicates(session, values)) return()
      
      update_tables(values$table_id, values, input, debug)
      
      table_code_collection <-
        structure(list(package_version = packageVersion("cbsots"),
                       table_code = values$tables),
                  class = "table_code_collection")
      
      if (debug) {
        cat("saving table_codes\n")
        print(table_code_collection)
      }
      
      saveRDS(table_code_collection, file = output_file)
    })
    
    
    # For the moment ignored.
    # observeEvent(input$searchField, {
    #   
    #   search <- input$searchField
    #   name <- input$selected_tab
    #   
    #   if (trimws(search) == "") {
    #     orig_key_order <- values$tables[[values$table_id]]$codes[[name]]$OrigKeyOrder
    #     values[[name]] <- order_code_rows(values[[name]], orig_key_order)
    #     return(invisible(NULL))
    #   }
    #   
    #   keys <- values[[name]]$Key
    #   codes <- values[[name]]$Code
    #   titles <- values[[name]]$Title
    # 
    #   key_index <- grep(search, keys, ignore.case = TRUE)
    #   code_index <- grep(search, codes, ignore.case = TRUE)
    #   title_index <- grep(search, titles, ignore.case = TRUE)
    #   all <- union(key_index, title_index)
    #   all <- union(all, code_index)
    #   if (length(all) == 0) return(invisible(NULL))
    # 
    #   rest <- setdiff(seq_along(keys), all)
    #   order <- c(all, rest)
    #   values[[name]] <- isolate(values[[name]][order, ])
    # })
  }
  
  app_list <- list(ui = ui, server = server)
  
  if (use_browser) {
    runApp(app_list, launch.browser = TRUE)
  } else {
    runApp(app_list)
  }
    
  return(invisible(NULL))
}


#
# help functions
#

check_duplicates <- function(session, values) {
  for (name in values$names) {
    codes <- values[[name]]$Code[values[[name]]$Select]
    if (anyDuplicated(codes)) {
      dupl <- codes[duplicated(codes)]
      showModal(modalDialog(
        title = "Duplicates in code",
        HTML(paste0("Duplicate code for the selected keys of ", name,
                    "<br>Duplicatecodes:\n", paste(dupl, collapse = ", "),
                    "<br>Please correct before proceding.")),
        easyClose = TRUE
      ))
      updateSelectInput(session, "table_description",
                        selected = values$table_description_dict[values$table_id])
      return(TRUE)
    }
  }
 return(FALSE)
}


update_tables <- function(table_id, values, input, debug) {
  
  old <- values$tables[[table_id]]

  if (is.null(old)) {
    # this situation occurs when a table has been deleted
    return(invisible(NULL))
  }

  # ordering
  values$tables[[table_id]]$order <- input$order_input_order

  # tables
  for (name in values$names) {
    orig_key_order <- values$tables[[table_id]]$codes[[name]]$OrigKeyOrde
    values$tables[[table_id]]$codes[[name]][ ,1:4] <- 
                            order_code_rows(values[[name]], orig_key_order)
  }
  
  if (!isTRUE(all.equal(old, values$tables[[table_id]]))) {
    values$tables[[table_id]]$last_modified <- Sys.time()
  }
  
  if (debug) {
    cat(sprintf("\n\nSaving current values for table %s\n", table_id))
    print(values$tables[[table_id]])
  }
  
  return(invisible(NULL))
}

