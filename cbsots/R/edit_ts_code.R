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
  
  if (length(tables) > 0) {
    short_titles <- sapply(tables, FUN = function(x) return(x$short_title))
    table_descriptions <- paste(names(tables), "-", short_titles)
  } else {
    table_descriptions <- character(0)
  }
 
  ui <- pageWithSidebar(
    
    headerPanel('CBS Timeseries Coding'),
    sidebarPanel(
      selectInput("table_description",
                  label = "Open a table for editing",
                  choices = table_descriptions,
                  selected = table_descriptions[1]),
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
    
    values <- reactiveValues(tables = tables, 
                             table_descriptions = table_descriptions,
                             table_id = NA_character_,
                             table_desc = NA_character_, 
                             prev_table_id = NA_character_,
                             prev_table_desc = NA_character_)
    
    # conversion table table_descriptions -> table_ids
    if (length(tables) > 0) {
      table_ids <- names(tables)
      names(table_ids) <- table_descriptions
    } else {
      table_ids <- character(0)
    }
    values$table_ids <- table_ids
    
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
          # first reset the selection, then return
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
      
      new_table_ids <- table_info$Identifier
      names(new_table_ids) <- new_table_descriptions
      values$new_table_ids <- new_table_ids
   
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

      new_table_desc <- input$new_table_description
      new_table_id <- get_table_id(new_table_desc, values$new_table_ids)
      if (is.na(new_table_id)) {
        return(invisible(NULL))
      }
      
      # extra test
      if (new_table_id %in% values$table_ids) {
        showModal(modalDialog(
          title = "Internal error",
          paste0("Table \"", new_table_desc, "\" already present in current tables\n"),
          easyClose = TRUE
        )) 
        cat("Table \"", new_table_desc, "\" already present")
        cat("Current list of tables\n")
        print(table_ids)
        return(invisible(NULL))
      }
    
      # add new tables
      values$tables[[new_table_id]] <- create_new_table(new_table_id)
      
      values$table_descriptions <- c(values$table_descriptions, new_table_desc)
      values$table_ids[new_table_desc] <- new_table_id
      
      # reorder the tables
      ord <- order(values$table_ids)
      values$tables <- values$tables[ord]
      values$table_descriptions <- values$table_descriptions[ord]
      values$table_ids <- values$table_ids[ord]
    
      updateSelectInput(session, inputId = "table_description",
                        choices = values$table_descriptions,
                        selected = new_table_desc)
      
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
      
      delete_table_id <- get_table_id(input$delete_table_description,
                                      values$table_ids)
      if (is.na(delete_table_id)) {
        return(invisible(NULL))
      }
      
      values$tables[[delete_table_id]] <- NULL
      
      if (!is.na(values$prev_table_id) && 
          values$prev_table_id == delete_table_id) {
        values$prev_table_id   <- NA_character_
        values$prev_table_desc <- NA_character_
      }
  
      # update table_descriptions
      idx <- pmatch(delete_table_id, values$table_ids)
      values$table_descriptions <- values$table_descriptions[-idx]
      values$table_ids <- values$table_ids[-idx]
      
      if (delete_table_id == values$table_id) {
        # if the table that is now shown is deleted, we have to select
        # a new table
        if (!is.na(values$prev_table_desc)) {
          new_table_desc <- values$prev_table_desc
        } else {
          new_table_desc <- values$table_descriptions[1]
        }
      } else {
        new_table_desc <- values$table_desc
      }
      
      updateSelectInput(session, inputId = "table_description",
                        choices = names(values$table_ids),
                        selected = new_table_desc)
      
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
