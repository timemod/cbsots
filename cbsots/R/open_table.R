# This function opens a new table. All information about the table is contained 
# in argument values.
open_table <- function(values, input, output, selected_tab = NULL, debug) {
  
  if (debug) {
    cat(sprintf("In function open_table, table_id =  %s.\n", values$table_id))
  }
  
  table_id <- values$table_id
  tab_names <- values$tab_names
 
  #
  # create the tabels
  #
  
  # create the table for the first dimension (the Topic)
  tab_index <- if (!is.null(selected_tab)) {
                  match(selected_tab, tab_names)
                } else {
                  1
                }
  
  hot_id <- get_hot_id(table_id, tab_names[tab_index])
  tab <- values$ts_code[[table_id]]$codes[[tab_index]]
  output[[hot_id]] <- renderCodetable(codetable(tab))
  
  # create empty tables for the other dimensions, the real tables will
  # actually be created when the tab selection changes.
  if (length(tab_names) > 1) {
    make_empty_table <- function(name) {
      hot_id <- get_hot_id(table_id, name)
      output[[hot_id]] <- NULL
      return()
    }
    lapply(tab_names[-tab_index], FUN = make_empty_table)
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
            print(head(values$ts_code[[values$table_id]]$codes[[name]][, 1:3]))
            cat("current values\n")
            print(head(df_input[, 1:3]))
          }
          values$ts_code[[values$table_id]]$codes[[name]][, 1:4] <- df_input
        } else if (debug) {
          cat(paste0("Something is wrong with input$", hot_id, ", 
                     check warnings\n"))
        }
      } else if (debug) {
        cat(paste0("input$", hot_id, " is NULL ...\n"))
      }
    })
  }
  
  lapply(tab_names, make_observer)
  
  # 
  # prepare tabbed pane
  #
  
  make_panel <- function(name) {
    hot_id <- get_hot_id(table_id, name)
    return(tabPanel(name, codetableOutput(hot_id)))
  }
  
  output$table_pane <- renderUI({
    # add isolate, otherwise the table_pane will be redrawn every time that
    # values$ts_code changes
    isolate({
      myTabs <- lapply(tab_names, make_panel)
      ret <- list(h3(paste("Tabel", values$table_desc)), br(),
                  orderInput(inputId = "order_input", 
                             label = paste("Order of dimensions used to create",
                                            "names (drag and drop items to",
                                            "change order):"), 
                             items = values$ts_code[[table_id]]$order),
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
