#' Edit timeseries codes
#' 
#' @param output_file the name of a file where the timeseries coding is stored.
#' The filename usually has extension \code{.rds}.
#' @param input_file a filename with timeseries codes. This file does not have
#' to exist yet. If specified, it should be an rds file 
#' containing a \code{table_code_collection} object.
#' @param use_browser if \code{TRUE}, then display the graphical user interface
#'  in the browser. Otherwise the RStudio viewer is used.
#' @param a logical. If \code{TRUE}, then use the debugging mode
#'  (only for developpers)
#' @import shiny
#' @import rhandsontable
#' @import shinyjqui
#' @import cbsodataR
#' @importFrom utils packageVersion
#' @export
edit_ts_code <- function(output_file, input_file, use_browser = TRUE, 
                         debug = FALSE) {
  
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
    
    values <- reactiveValues(tables = tables, table_id = NA_character_,
                             table_desc = NA_character_, 
                             prev_table_id = NA_character_,
                             prev_table_desc = NA_character_)
    
    if (length(tables) > 0) {
    
      short_titles <- sapply(tables, FUN = function(x) return(x$short_title))
      table_descriptions <- paste(names(tables), "-", short_titles)
      
      #
      # conversion table table_descriptions -> table_ids
      #
      table_ids <- names(tables)
      names(table_ids) <- table_descriptions
      
      values$table_descriptions <- table_descriptions
      values$table_ids <- table_ids
    } else {
      values$table_descriptions <- character(0)
      values$table_ids <- character(0)
    }
    
    output$table_chooser <- renderUI({
      selectInput("table_description",
                  label = "Choose a table for editing",
                  # isolate values$table_descriptions: if 
                  # value$table_descriptions changes also 
                  # values$selec_table_desc
                  # changes, and we do not want double events.
                  choices = isolate(values$table_descriptions),
                  selected = values$select_table_desc)
    })
    
    observeEvent(input$table_description, {
      
      if (debug) cat("table_description changed\n")
      
      cat("table_description changed\n")
      cat(sprintf("input$table_description = %s\n", input$table_description))
      cat(sprintf("aantal tabellen = %d\n", length(values$tables)))
      
      if (length(values$tables) == 0) {
        output$tabel <- renderUI({return(NULL)})
        return(invisible(NULL))
      }
      
      # check for duplicates in current table, otherwise don't change
      if (!is.null(values$table_id)) {
        if (check_duplicates(session, values)) {
          # first rest the selection, then return
          updateSelectInput(session, "table_description", 
                            selected = values$table_description)
          return()
        }
      }
  
      open_table(input$table_description, values, input, output, debug)
      
    })  # table_description_event
    
    
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
    
    observeEvent(input$new_table_ok, {

      new_table_id <- values$new_table_id_dict[input$new_table_description]
      new_table_id <- as.character(new_table_id)
      
      values$tables[[new_table_id]] <- create_new_table(new_table_id)
      values$tables <- values$tables[sort(names(values$tables))]
      
      short_titles <- sapply(values$tables, FUN = function(x) return(x$short_title))
      values$table_descriptions <- paste(names(values$tables), "-", short_titles)
      
      #
      # conversion tables between table_descriptions -> table_ids
      #
      values$table_ids <- names(values$tables)
      names(values$table_ids) <- values$table_descriptions
      
      # this statement will trigger the Open Table chooser
      values$select_table_desc <- input$new_table_description
      
      removeModal()
    })
    
    
    deleteTableModal <- function(failed = FALSE) {
     
      modalDialog(
        selectInput("delete_table_description", 
                    label = "Delete a table",
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
      if (length(values$tables) == 0) {
        showModal(modalDialog(
          title = "No tables to delete",
          HTML(paste0("There are no tables to delete")),
          easyClose = TRUE
        )) 
      } else {
        showModal(deleteTableModal())
      }
    })
    
    observeEvent(input$delete_table_ok, {
      
      delete_table_desc <- input$delete_table_description
      delete_table_id <- values$table_ids[delete_table_desc]
      delete_table_id <- as.character(delete_table_id)
      
      values$tables[[delete_table_id]] <- NULL
      
      if (!is.na(values$prev_table_id) && 
          values$prev_table_id == delete_table_id) {
        values$prev_table_id   <- NA_character_
        values$prev_table_desc <- NA_character_
      }
      
      values$table_descriptions <- setdiff(values$table_descriptions, 
                                           delete_table_desc)
   
      #
      # conversion tables between table_descriptions -> table_ids
      #
      values$table_ids <- names(values$tables)
      names(values$table_ids) <- values$table_descriptions
      
      if (delete_table_id == values$table_id) {
          
        cat("deleting current table\n")
        cat("table_id = %d\n", values$table_id)
        cat("prev_table_id = %d\n", values$prev_table_id)
        
        if (!is.na(values$prev_table_desc)) {
          new_table_desc <- values$prev_table_desc
        } else {
          new_table_desc <- values$table_descriptions[1]
        }
 
        values$select_table_desc <- new_table_desc
      }
      
      removeModal()
    })
    
  

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
      
      return(TRUE)
    }
  }
  return(FALSE)
}
