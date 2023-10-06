#' @import htmlwidgets
codetable <- function(data, width = NULL, height = NULL) {
  
  if (is.null(data))  {
    
    # NOTE: errors in shinyWidgets constructors are turned into warnings,
    # and the error is shown in red at the position were the widget should
    # be rendered.
    stop("Internal error: no data available")
    
  } else if (ncol(data) < 4) {
    
    stop("Internal error: data has less than 4 columns")
    
    # TODO: also check column names and column types? 
    
  } else {
      
    data <- data[, 1:4]
    
    # Remove NA values in data$Code, sometimes there is an NA value in this 
    # column. It is not clear what causes this problem, but it may result
    # in serious problems.
    data$Code[is.na(data$Code)] <- ""
  
    if (any(is.na(data))) {
      # See the note about errors above.
      stop("Internal error: data contains NA values")
    }
  }

  x <- list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE)
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
