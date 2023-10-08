newTableInput <- function(id) {
  tagList(
    h3("Create new code table"),
    p(),
    actionButton(NS(id, "new_table"), "New table"),
  )
}

newTableServer <- function(id, table_descs, tscod, base_url, debug) {
  
  # Function that creates a dialog asking for a table and an optional base table.
  select_new_table_dialog <- function(table_descriptions, 
                                      base_table_descriptions) {
    
    choices <- create_table_choices(table_descriptions)
    base_table_choices <- c("Do not use base table" = "", 
                            base_table_descriptions)
    
    dialog <- modalDialog(
      h3("New Table"),
      "You can enter a search query in the text field below.",
      "When necessary, use Backspace to erase the text field.",
      selectizeInput(NS(id, "new_table_desc"), label = "",
                     choices = NULL, width = "200%"),
      p(),
      "Optionally specify an existing table (the \"base table\") used to fill in",
      "the new table. Leave the text field empty to create an empty new table.",
      "When necessary, use Backspace to erase the text field.",
      selectizeInput(NS(id, "new_table_base_desc"), label = "",
                     choices = NULL, width = "200%"),
      p(),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(NS(id, "new_table_ok"), "OK")
      ),
      easyClose = TRUE
    )
    
    updateSelectizeInput(
      inputId = "new_table_desc", choices = choices,
      options = list(maxOptions = length(choices)),
      server = TRUE
    )

    updateSelectizeInput(
      inputId = "new_table_base_desc",
      choices = base_table_choices,
      options = list(
        maxOptions =
          length(base_table_choices)
      ),
      server = TRUE
    )

    return(dialog)
  }
  
  moduleServer(id, function(input, ouput, servers) {
    
    r_values <- reactiveValues()
    
    observeEvent(input$new_table, {
      if (debug) cat("\nnewTableServer: new table button pressed\n")
      old_table_descs <- table_descs()
      old_table_ids <- names(old_table_descs)
      print(old_table_ids)
      
      shinybusy::show_modal_spinner(text = "Downloading ...")
      new_table_descs <- get_new_table_descs(old_table_ids, base_url)
      shinybusy::remove_modal_spinner()
      
      if (is.null(new_table_descs)) {
        shinyalert("Error", "Error downloading list of tables" , type = "error")
      } else {
        showModal(select_new_table_dialog(new_table_descs, table_descs()))
      } 
    })
    
    observeEvent(input$new_table_ok, {
      new_table_desc <- input$new_table_desc
      if (new_table_desc == "") {
        return()
      }
      new_table_id <- get_table_id(new_table_desc)
      
      # add new table
      tryCatch({
    
        shinybusy::show_modal_spinner(text = "Downloading ...")
        tblcod_new <- table_code(new_table_id, base_url)
        shinybusy::remove_modal_spinner()
        
        # check if there is a base table
        
        base_table_desc <-  input$new_table_base_desc
        if (base_table_desc != "") {
          base_table_id <- get_table_id(base_table_desc)
          base_table <- tscod()[[base_table_id]]
          ret <- call_update_table(r_values$tblcod_new, base_table, new_table_id,
                                   base_table_id)
          if (is.null(ret)) return()
          tblcod_new <- ret$new_table
          if (length(ret$warnings) > 0) {
            r_values$tblcod_new_candidate <- tblcod_new
            showWarningsDialog(ret$warnings, NS(id, "warnings_ok"))
          } else {
            r_values$tblcod_new <- tblcod_new
            removeModal()
          }
        } else {
          r_values$tblcod_new <- tblcod_new
          removeModal()
        }
      }, error = function(e) {
        cat("error\n")
        print(e)
        shinyalert("Error", paste("Error while downloading table", 
                                  new_table_id) , 
                   type = "error")
      })
    })
    
    observeEvent(input$warnings_ok, {
      r_values$tblcod_new <- tblcod_new_candidate
      removeModal()
    })
    
    
    return(reactive(r_values$tblcod_new))
  })   
}