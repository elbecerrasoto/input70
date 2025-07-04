library(shiny)
source("ui_helper.R")

ui <- fluidPage(
  titlePanel("Leontief Input Output Modeling of the Mexico States"),
  simulateRow,
  bigboxRow,
  inputRow,
  downloadRow
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
