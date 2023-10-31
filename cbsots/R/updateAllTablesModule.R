updateAllTablesInput <- function(id, button_width) {
  actionButton(NS(id, "update"), "Update All Tables",
    style = sprintf("width: %s", button_width)
  )
}

updateAllTablesServer <- function(id, table_present, tscod, base_url, debug) {
  moduleServer(id, function(input, ouput, session) {
    
    r_values <- reactiveValues()
    
    observeEvent(table_present(), {
      if (debug) {
        cat("\nupdateAllTablesServer: table_present changed, new value: ", 
          table_present(), "\n\n")
      }
      if (table_present()) {
        shinyjs::enable("update")
      } else {
        shinyjs::disable("update")
      }
      return()
    })
    
    observeEvent(input$update, {
     if (debug) cat("\nupdateAllTablesServer: update button pressed\n\n")
     r_values$tsod_upd <- NULL
     showModal(modalDialog(
       title = "Confirm",
       HTML(paste0(
         "Do you want to update all tables",
         "<br>with recent table information on the CBS website?"
       )),
       footer = tagList(
         modalButton("No"),
         actionButton(NS(id, "update_all_tables_confirmed"), "Yes")
       ),
       easyClose = TRUE
     ))
   })
    
    observeEvent(input$update_all_tables_confirmed, {
      if (debug) cat("\nUpdate all tables confirmed\n")
      removeModal()
      tscod_old <- tscod()
      retval <- perform_update_all_tables(tscod_old, base_url = base_url,
                                          debug = debug)
      
      tscod_upd <- retval$ts_code_upd
      if (identical(tscod_upd, tscod_old)) {
        if (debug) cat("\nAll tables are already up to date, nothing to do\n\n")
        return()
      }
      warning_ids <- retval$warning_ids
      if (length(warning_ids) > 0) {
        r_values$tscod_upd_candidate <- tscod_upd
        wmsg <- paste("For tables", paste(warning_ids, collapse = ", "),
                      "some old keys do not match perfectly with new keys.\n",
                      "Check the match reports in directory 'match_reports'.")
        wmsg <- strwrap(wmsg, width = 80)
        wmsg <- paste(wmsg, collapse = "\n")
        showWarningsDialog(wmsg, NS(id, "accept_warnings"))
      } else {
        r_values$tscod_upd <- tscod_upd
      }
      return()
    })
    
    observeEvent(input$accept_warnings, {
      if (debug) cat("\nupdateAllTablesServer: accept_warnings\n\n")
      removeModal()
      r_values$tscod_upd <- r_values$tscod_upd_candidate
    })
    
    return(reactive(r_values$tscod_upd))
  })   
}
