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
  
  #
  # create the tabels
  #
  
  make_table <- function(name) {
    output[[name]] <- 
      if (!is.null(isolate(values[[name]]))) {
        render_table(isolate(values[[name]]))
      } else {
        codetable(as.data.frame("dummy"))
      }
    return(invisible(NULL))
  }
  
  lapply(values$names, FUN = make_table)
  
  

  # 
  # observers for the tables
  #
  make_observer <- function(name) {
    
    observeEvent(input[[name]], {
      if (debug) cat(paste("table", name , "changed\n"))
      if (!is.null(input[[name]])) {
        df_values <- values[[name]]
        df_input <- convert_codetable(input[[name]], colnames(df_values))
        if (!is.null(df_values) && !is.null(df_input) &&
            # only respond to changes in Select or Code
            identical(df_input$Key, df_values$Key) &&
            identical(df_input$Title, df_values$Title)) {
          
          if (debug) {
            cat("current values\n")
            print(head(df_input[, 1:3]))
          }
          values[[name]] <- df_input
          if (debug) {
            cat("new values\n")
            print(head(values[[name]][, 1:3]))
          }
        }
      }
    })
  }
  
  lapply(values$names, make_observer)
  
  # 
  # prepare tabbed pane
  #
  
  make_panel <- function(name) {
    return(tabPanel(name, codetableOutput(name)))
  }
  
  output$tabel <- renderUI({
    isolate({
      table_items <- values$names
      myTabs <- lapply(table_items, make_panel)
      
      ret <- list(h3(paste("Tabel", new_table_description)), br(),
                  p(), 
                  h5("Order used to create names"),
                  orderInput(inputId = "order_input", label = NULL, 
                             items = values$tables[[new_table_id]]$order),
                  p(),p(),
                  tags$div(
                    HTML("&#128270;"),
                    tags$input(type = "text", id = "search_field",
                               placeholder = "Search ..."),
                    tags$button(HTML("&#8249;"), class = "previous round", 
                                id = "prev_button"),
                    tags$button(HTML("&#8250;"), class = "next round", 
                                id = "next_button")
                  ),
                  p(),
                  do.call(tabsetPanel, c(list(id = "selected_tab"), myTabs)))
      
      return(ret)
    })
  })
  
  values$table_id <- new_table_id
  values$table_desc <- new_table_description
  
  return(invisible(NULL))
}
