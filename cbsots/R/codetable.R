#' @import htmlwidgets
#' @export
codetable <- function(data, width = NULL, height = NULL) {
  
  cat("codetable aangeroepen\n")
  
  # fix NA values in data$code, sometimes a value became NA, for unknown reasons
  data$Code[is.na(data$Code)] <- ""
  
   x <- list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE,
                            digits = digits)
  )
  

  # create widget
  htmlwidgets::createWidget(
    name = 'codetable', x, width = width, height = height, package = 'cbsots',
    elementId = NULL)
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
#' @export
codetableOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'codetable', width, height, 
                                 package = 'cbsots')
}

#' @export
renderCodetable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, codetableOutput, env, quoted = TRUE)
}
