rm(list = ls())
library(shiny)
library(rhandsontable)

df <- data.frame(Key = c("A", "B", "C", "D", "E", "F"),
                Select = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                Code = c("p", "q", "r", "s", "t", "u"),
                Title = c("Aap", "Noot", "Mies", "Centraal Planbureau", "Piet", "Klaas"))
df_small <- df[1:2, ]

maak_app <- function() {
  ui <- fluidPage(
    br(),
    actionButton("show_data", "Show data with handsontable"),
    actionButton("show_mini_data", "Show small selection of data with handsontable"),
    rHandsontableOutput("hot")
  )
  server <- function(input, output, session) {
    observeEvent(input$show_data, {
      output$hot <- renderRHandsontable(rhandsontable(df))  
    })
    observeEvent(input$show_mini_data, {
      output$hot <- renderRHandsontable(rhandsontable(df_small))  
    })
    observeEvent(input$hot, {
      cat("\nDe tabel is gewijzigd\n\n")
      #print(input$hot)
    })
  }
  return(shinyApp(ui, server))
}

app <- maak_app()
runApp(app, launch.browser = TRUE)