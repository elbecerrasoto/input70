library(shiny)

simulateRow <- fluidRow(
  column(2),
  column(8),
  column(2), radioButtons("shock_type", "Choose the type of Demand Shock to inject:",
    choiceNames = list(
      "Add (millions of pesos)",
      "Multiply by previous demand",
      "Input a total demand"
    ),
    choiceValues = list("add", "multiply", "raw")
  )
)

bigboxRow <- fluidRow(
  column(3, textOutput("gdp_state_box")),
  column(3, textOutput("employment_state_box")),
  column(3, textOutput("gdp_rest_box")),
  column(3, textOutput("employment_rest_box")),
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
