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
  
  if (!is.na(values$table_id)) {
    # another table now open
    if (values$table_id == new_table_id) {
      return(invisible(NULL))
    } else {
      # save ordering
      values$tables[[values$table_id]]$order <- input$order_input_order
    }
  }
  
  # update new tab names for the current table
  values$names <- names(values$tables[[new_table_id]]$codes)
  
  #
  # create the tabels
  #
  
  make_table <- function(name) {
    tab <- isolate(values$tables[[new_table_id]]$codes[[name]])
    tab <- tab[ , 1:4] 
    hot_id <- get_hot_id(new_table_id, name)
    output[[hot_id]] <- renderCodetable(codetable(tab))
    return()
  }
  
  lapply(values$names, FUN = make_table)
  

  # 
  # observers for the tables
  #
  make_observer <- function(name) {
    hot_id <- get_hot_id(new_table_id, name)
    observeEvent(input[[hot_id]], {
      if (debug) cat(paste("Table", hot_id , "has changed\n"))
      if (!is.null(input[[hot_id]])) {
        df_input <- convert_codetable(input[[hot_id]])
          if (debug) {
        if (!is.null(df_input)) {
            cat("current values\n")
            print(head(df_input[, 1:3]))
          }
          values$tables[[values$table_id]]$codes[[name]][, 1:4] <- df_input
        } else if (debug) {
          cat(paste0("Something is wrong with input$", hot_id, ", 
                     check warnings\n"))
        }
      } else if (debug) {
        cat(paste0("input$", hot_id, " is NULL ...\n"))
      }
    })
  }
  
  lapply(values$names, make_observer)
  
  # 
  # prepare tabbed pane
  #
  
  make_panel <- function(name) {
    hot_id <- get_hot_id(new_table_id, name)
    return(tabPanel(name, codetableOutput(hot_id)))
  }
  
  output$tabel <- renderUI({
    isolate({
      myTabs <- lapply(values$names, make_panel)
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
