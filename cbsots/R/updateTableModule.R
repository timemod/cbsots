#
# This module handles the action for updating a single table.
#
updateTableInput <- function(id, button_width) {
  actionButton(NS(id, "update"), "Update This Table",
    style = sprintf("width: %s", button_width)
  )
}

updateTableServer <- function(id, table_open, tblcod, table_id, base_url, 
                              debug) {
  
  moduleServer(id, function(input, ouput, session) {
    
    r_values <- reactiveValues()
    
    observeEvent(table_open(), {
      if (debug) {
        cat("\nupdateTableServer: table_open changed, new value: ", 
            table_open(), 
          "\n\n")
      }
      r_values$tblcod_upd <- NULL
      if (table_open()) {
        shinyjs::enable("update")
      } else {
        shinyjs::disable("update")
      }
      return()
    })
    
    observeEvent(input$update, {
      if (debug) {
        cat(sprintf("\nupdateTableServer: update button pressed, table = %s\n\n",
                    table_id()))
      }
      showModal(modalDialog(
        title = "Confirm",
        HTML(paste0(
          "Do you want to update \"", table_id(), "\"",
          "<br>with recent table information on the CBS website?"
        )),
        footer = tagList(
          modalButton("No"),
          actionButton(NS(id, "update_confirmed"), "Yes")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$update_confirmed, {
      if (debug) cat("\nupdateTableServer: update confirmed\n")
    
      removeModal()
     
      tblcod_old <- tblcod()
      tbl_id <- tblcod_old$id
     
      show_modal_spinner(text = "Downloading list of tables ...")
      ret <- perform_update_table(tblcod_old, table_id = tbl_id, 
                                  base_url = base_url)
      remove_modal_spinner()
      if (is.null(ret)) {
        # something went wrong
        return()
      }
      
      tblcod_upd <- ret$table_code_upd
      if (identical(tblcod_old, tblcod_upd)) {
        if (debug) cat("\nThe tables is already up to date, nothing to do.\n\n")
        return()
      } else if (debug) cat("\n")
      
      if (length(ret$warnings) > 0) {
        r_values$tblcod_upd_candidate <- tblcod_upd
        showWarningsDialog(ret$warnings, NS(id, "accept_warnings"))
      } else {
        r_values$tblcod_upd <- tblcod_upd
      }
    })
    
    observeEvent(input$accept_warnings, {
      if (debug) cat("\nupdateTableServer: accept_warnings\n\n")
      removeModal()
      r_values$tblcod_upd <- r_values$tblcod_upd_candidate
    })
    
    return(reactive(r_values$tblcod_upd))
  })
}
