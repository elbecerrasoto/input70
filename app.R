library(shiny)

ui <- fluidPage(
  titlePanel("Simulator"),
  fluidRow(
    column(2),
    column(8),
    column(2)
  ),
  fluidRow(
    column(2),
    column(8),
    column(2)
  ),
  fluidRow(
    column(2),
    column(8),
    column(2)
  ),
  fluidRow(
    column(2),
    column(8),
    column(2)
  ),
  fluidRow(
    column(2),
    column(8),
    column(2)
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)
