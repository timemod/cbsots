# Test for handsontable in different tabs of a tabsetpanel.
# This does not work very well, after changing tabs the results do no look
# nicely. After clicking on the table the problem disappear.
# Note that it works better with package rhandsontable, although not perfectly.
rm(list = ls())
library(shiny)
library(cbsots)

df <- data.frame(Key = c("A", "B", "C", "D", "E", "F"),
                Select = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                Code = c("p", "q", "r", "s", "t", "u"),
                Title = c("Aap", "Noot", "Mies", "Centraal Planbureau", "Piet", "Klaas"))
               
df_small <- df[1:2, ]

maak_app <- function() {
  
  ui <- fluidPage(
    p(),
    icon("search"),
    tags$input(type = "text", id = "search_field",
               placeholder = "Search ..."),
    tags$button(icon("caret-left"), id = "prev_button"),
    tags$button(icon("caret-right"),  id = "next_button"),
    p(),
    tabsetPanel(
      id = "tabset",
      tabPanel("data", cbsots:::codetable(df)),
      tabPanel("data_small", cbsots:::codetable(df_small))
    )
  )
  
  server <- function(input, output, session) {
    session$onSessionEnded(shiny::stopApp)
  }
  return(shinyApp(ui, server))
}

app <- maak_app()
runApp(app, launch.browser = TRUE)