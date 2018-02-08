open_table <- function(new_table_description, values, input, output, debug) {
  
  #
  # open a new table
  #
  
  new_table_id <- get_table_id(new_table_description, values$table_ids)
  if (is.na(new_table_id)) {
    return(invisible(NULL))
  }
 
  if (debug) {
    cat(sprintf("Opening table with id = %s\n", new_table_id))
  }
  
  # save current table if present
  if (!is.na(values$table_id)) {
  
    if (values$table_id == new_table_id) {
      return(invisible(NULL))
    } 
    
    # save current table 
    update_tables(values$table_id, values, input, debug)
    
    # remove previous dimensions (not Topic)
    for (dimension in values$names[-1]) {
      values[[dimension]] <- NULL
    }
  }
  
  # copy new tables
  values$names <- names(values$tables[[new_table_id]]$codes)
  dimensions <- values$names[-1]
  for (name in values$names) {
    values[[name]] <- values$tables[[new_table_id]]$codes[[name]][, 1:4]
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
    
    # 
    # prepare tabbed pane
    #
    
    make_panel <- function(name) {
      return(tabPanel(name, rHandsontableOutput(name)))
    }
    
    output$tabel <- renderUI({
      isolate({
        table_items <- values$names
        myTabs <- lapply(table_items, make_panel)
        
        ret <- list(h2(paste("Tabel", new_table_description)), br(),
                    list(p()), 
                    orderInput(inputId = "order_input", 
                               label = "Order for name generation", 
                               items = values$tables[[new_table_id]]$order),
                    list(p()),
                    textInput(inputId = "searchField",  
                              label = "Search in tabel (enter a text followed by ENTER)"),
                    p(), h3("Codes"),
                    do.call(tabsetPanel, c(list(id = "selected_tab"), myTabs)))
        
        return(ret)
      })
    })
  }
  
  lapply(values$names, make_observer)
  
  # table administration
  if (!is.na(values$table_desc)) {
    values$prev_table_stack <- c(values$prev_table_stack, values$table_desc)
  }
  values$table_id <- new_table_id
  values$table_desc <- new_table_description
  
  return(invisible(NULL))
}
