rm(list = ls())
library(shiny)
library(cbsots)

df <- data.frame(Key = c("A", "B", "C", "D", "E", "F"),
                Select = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                Code = c("p", "q", "r", "s", "t", "u"),
                Title = c("Aap", "Noot", "Mies", "Centraal Planbureau", "Piet", "Klaas"))
               
df_small <- df[1:2, ]

df_error <- df
df_error[3, "Key"] <- NA

maak_app <- function() {
  ui <- fluidPage(
    p(),
    actionButton("show_data", "Show data with cbsots"),
    actionButton("show_small_data", "Show small selection of data with cbsots"),
    actionButton("show_error_data", "Show data with error"),
    p(),
    icon("search"),
    tags$input(type = "text", id = "search_field",
                 placeholder = "Search ..."),
    tags$button(icon("caret-left"), id = "prev_button"),
    tags$button(icon("caret-right"),  id = "next_button"),
    p(),
    cbsots:::codetableOutput("hot")
  )
  server <- function(input, output, session) {
    session$onSessionEnded(shiny::stopApp)
    observeEvent(input$show_data, {
      output$hot <- cbsots:::renderCodetable(cbsots:::codetable(df))  
    })
    observeEvent(input$show_small_data, {
      output$hot <- cbsots:::renderCodetable(cbsots:::codetable(df_small))  
    })
    observeEvent(input$show_error_data, {
      output$hot <- cbsots:::renderCodetable(cbsots:::codetable(df_error))  
    })
    observeEvent(input$hot, {
      cat("\nDe tabel is gewijzigd\n\n")
      print(input$hot)
    })
  }
  return(shinyApp(ui, server))
}

app <- maak_app()
runApp(app, launch.browser = TRUE)