library(shiny)

simulateRow <- fluidRow(
  column(2),
  column(8),
  column(2)
)

bigboxRow <- fluidRow(
  column(2),
  column(8),
  column(2)
)

inputRow <- fluidRow(
  column(2),
  column(8),
  column(2)
)

downloadRow <- fluidRow(
  column(2),
  column(8),
  column(2)
)

ui <- fluidPage(
  titlePanel("Simulator"),
  simulateRow,
  bigboxRow,
  inputRow,
  downloadRow
)


# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)
