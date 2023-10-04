#' @import htmlwidgets
codetable <- function(data, id = NULL, width = NULL, height = NULL) {

  if (is.null(data))  {
    data <- as.data.frame("Internal error: no data available")
  } else {
      
    data <- data[ , 1:4]
    
    # remove NA values in data$Code, sometimes there is an NA value in this 
    # column. It is not clear what caused that problem, but is may result
    # is serious problems
    data$Code[is.na(data$Code)] <- ""
  
    if (any(is.na(data))) {
      stop("Internal error in codetable... data contains NA values")
    }
  }
  
  x <- list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE)
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'codetable', x, width = width, height = height, package = 'cbsots',
    elementId = id)
}

# Shiny bindings for code_table
#
# Output and render functions for using code_table within Shiny
# applications and interactive Rmd documents.
#
# @param outputId output variable to read from
# @param width,height Must be a valid CSS unit (like \code{'100\%'},
#   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#   string and have \code{'px'} appended.
# @param expr An expression that generates a code_table
# @param env The environment in which to evaluate \code{expr}.
# @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#   is useful if you want to save an expression in a variable.
#
codetableOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'codetable', width, height, 
                                 package = 'cbsots')
}

renderCodetable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, codetableOutput, env, quoted = TRUE)
}
