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
    tabsetPanel(
      id = "tabset",
      tabPanel("panel1", rhandsontable(df, id = "tab1")),
      tabPanel("panel2", rhandsontable(df_small, id = "tab2"))
    )
  )
  server <- function(input, output, session) {
    observeEvent(input$tab1, {
      cat("\nTabel 1 is gewijzigd\n\n")
      print(input$tab1)
    })
    observeEvent(input$tab2, {
      cat("\nTabel 2 is gewijzigd\n\n")
      print(input$tab2)
    })
  }
  return(shinyApp(ui, server))
}

app <- maak_app()
runApp(app, launch.browser = TRUE)
runApp(app)