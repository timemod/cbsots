#' @import htmlwidgets
codetable <- function(tblcod, table_id, dimension, selection,
                      width = NULL, height = NULL) {

  
  if (is.null(tblcod))  {
    
    # NOTE: errors in shinyWidgets constructors are turned into warnings,
    # and the error is shown in red at the position were the widget should
    # be rendered.
    validate("Internal error: no tblcod available")
    
  } else if (!identical(colnames(tblcod)[1:4], 
                        c("Key", "Select", "Code", "Title"))) {
    
  } else {
      
    tblcod <- tblcod[, 1:4]
    
    # Remove NA values in tblcod$Code, sometimes there is an NA value in this 
    # column. It is not clear what causes this problem, but it may result
    # in serious problems.
    tblcod$Code[is.na(tblcod$Code)] <- ""
  
    if (any(is.na(tblcod))) {
      # See the note about errors above.
      validate("Internal error: tblcod contains NA values")
    }
  }

  if (is.null(selection)) {
    selection <- list(row = 0, column = 0, row2 = 0, column2 = 0)
  } else {
    nr <- nrow(tblcod)
    nc <- ncol(tblcod)
    selection$row <- max(0, min(selection$row, nr - 1))
    selection$row2 <- max(0, min(selection$row2, nr - 1))
    selection$column <- max(0, min(selection$column, nc - 1))
    selection$column2 <- max(0, min(selection$column2, nc - 1))
  }
    
  x <- list(
    data = jsonlite::toJSON(tblcod, na = "string", rownames = FALSE),
    table_id = table_id,
    dimension = dimension,
    selection = selection
  )
  
  # create widget
  htmlwidgets::createWidget("codetable", x, width = width, height = height, 
                            package = "cbsots")
}

#
# Shiny bindings for code_table
#

codetableOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "codetable", width, height, 
                                 package = "cbsots")
}

renderCodetable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, codetableOutput, env, quoted = TRUE)
}
