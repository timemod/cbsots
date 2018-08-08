# This function open a new table. All information about the table is contained in
# argument values
open_table <- function(values, input, output, selected_tab = NULL, debug) {
  
  if (debug) {
    cat(sprintf("In function open_table, table_id =  %s.\n", values$table_id))
  }
  
  table_id <- values$table_id
  table_desc <- values$table_desc

  # update new tab names for the current table
  values$names <- names(values$tables[[table_id]]$codes)
  
  #
  # create the tabels
  #
  
  # create the table for the first dimension (the Topic)
  name_index <- if (!is.null(selected_tab)) {
                  match(selected_tab, values$names)
                } else {
                  1
                }
  
  hot_id <- get_hot_id(table_id, values$names[name_index])
  tab <- values$tables[[table_id]]$codes[[name_index]]
  output[[hot_id]] <- renderCodetable(codetable(tab))
  
  # create empty tables for the other dimensions, the real tables will
  # actually be created when the tab selection changes.
  if (length(values$names) > 1) {
    make_empty_table <- function(name) {
      hot_id <- get_hot_id(table_id, name)
      output[[hot_id]] <- NULL
      return()
    }
    lapply(values$names[-name_index], FUN = make_empty_table)
  }

  # 
  # observers for the tables
  #
  make_observer <- function(name) {
    hot_id <- get_hot_id(table_id, name)
    observeEvent(input[[hot_id]], {
      if (debug) cat(paste("Table", hot_id , "has changed\n"))
      if (!is.null(input[[hot_id]])) {
        df_input <- convert_codetable(input[[hot_id]])
        if (!is.null(df_input)) {
          if (debug) {
            cat("old values\n")
            print(head(values$tables[[values$table_id]]$codes[[name]][, 1:3]))
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
    hot_id <- get_hot_id(table_id, name)
    return(tabPanel(name, codetableOutput(hot_id)))
  }
  
  output$table_pane <- renderUI({
    # add isolate, otherwise the table_pane will be redrawn every time that
    # values$tables changes
    isolate({
      myTabs <- lapply(values$names, make_panel)
      ret <- list(h3(paste("Tabel", table_desc)), br(),
                  p(), 
                  h5("Order used to create names"),
                  orderInput(inputId = "order_input", label = NULL, 
                             items = values$tables[[table_id]]$order),
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
                  do.call(tabsetPanel, c(list(id = "tabsetpanel", 
                                            selected = selected_tab), myTabs)))
    })
    return(ret)
  })
  
  return(invisible(NULL))
}
