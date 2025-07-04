library(shiny)
source("ui_helper.R")

ui <- fluidPage(
  titlePanel("Simulator"),
  simulateRow,
  bigboxRow,
  inputRow,
  downloadRow
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
