# Test for handsontable in different tabs of a tabsetpanel.
# This does not work very well, after changing tabs the results do no look
# nicely. After clicking on the table the problem disappear.
# Note that it works better with package rhandsontable, although not perfectly.
rm(list = ls())
library(shiny)
library(cbsots)

df <- data.frame(
  Key = c("A", "B", "C", "D", "E", "F"),
  Select = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
  Code = c("p", "q", "r", "s", "t", "u"),
  Title = c("Aap", "Noot", "Mies", "Centraal Planbureau", "Piet", "Klaas")
)

df_small <- df[1:2, ]

table_id <- "1NED"
dimension <- "Topic"

maak_app <- function() {
  ui <- fluidPage(
    actionButton("show_inputs", "Show Inputs"),
    p(),
    icon("search"),
    tags$input(
      type = "text", id = "search_field",
      placeholder = "Search ..."
    ),
    tags$button(icon("caret-left"), id = "prev_button"),
    tags$button(icon("caret-right"), id = "next_button"),
    p(),
    tabsetPanel(
      id = "tabset",
      tabPanel("data", cbsots:::codetableOutput("hot")),
      tabPanel("data_small", cbsots:::codetableOutput("hot_small"))
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(shiny::stopApp)

    r_values <- reactiveValues(df = df, df_small = df_small)

    observeEvent(r_values$df, {
      cat("\n df changed event\n")
      output$hot <- cbsots:::renderCodetable(cbsots:::codetable(r_values$df,
        table_id = table_id,
        dimension = dimension
      ))
      # the next statement makes it worse:
      # outputOptions(output, "hot", suspendWhenHidden = FALSE)
    })

    observeEvent(r_values$df_small, {
      output$hot_small <- cbsots:::renderCodetable(cbsots:::codetable(r_values$df_small,
        table_id = table_id,
        dimension = dimension
      ))
      # the next statement makes it worse:
      # outputOptions(output, "hot_small", suspendWhenHidden = FALSE)
    })

    observeEvent(input$show_inputs, {
      cat("\nData:\n\n")
      print(input$hot)
      cat("\nData small:\n\n")
      print(input$hot_small)
    })
  }
  return(shinyApp(ui, server))
}

app <- maak_app()
runApp(app, launch.browser = TRUE)
