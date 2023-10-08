deleteTableInput <- function(id, button_width) {
  tagList(
    h3("Delete Code Table"),
    actionButton(NS(id, "delete"), "Delete Table",
      style = sprintf("width: %s", button_width))
                 
  )
}

deleteTableServer <- function(id, table_present, table_descs, debug) {
  
  moduleServer(id, function(input, ouput, servers) {
    
    r_values <- reactiveValues()
    
    observeEvent(table_present(), {
      if (debug) {
        cat("\ndeleteTablesServer: table_present changed, new value: ", 
            table_present(), "\n\n")
      }
      if (table_present()) {
        shinyjs::enable("delete")
      } else {
        shinyjs::disable("delete")
      }
      return()
    })
    
    observeEvent(input$delete, {
      if (debug) cat("\ndeleteTableServer: delete button pressed\n\n")
      showModal(select_table_dialog(
        NS(id, "delete"),
        "Delete Table",
        table_descs()
      ))
    })
    
    observeEvent(input$delete_ok, {
      delete_table_desc <- input$delete_desc
      if (delete_table_desc == "") return()
      delete_table_id <- get_table_id(delete_table_desc)
      delete_table_desc <- table_descs()[delete_table_id]
      r_values$delete_table_id_candidate <- delete_table_id
      
      showModal(modalDialog(
        title = "Confirm",
        HTML(paste0("Table \"", delete_table_desc, 
                    "\" will be permanently deleted",
                    "<br>Are you sure?")),
        footer = tagList(
          modalButton("No"),
          actionButton(NS(id, "delete_confirmed"), "Yes")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$delete_confirmed, {
      if (debug) cat("\ndelete_table_confirmed\n")
      r_values$delete_table_id <- r_values$delete_table_id_candidate
    })
  
    
    return(reactive(r_values$delete_table_id))
  })   
}