library(shiny)

simulate_row <- fluidRow(
  column(2),
  column(8),
  column(2)
)

bigbox_row <- fluidRow(
  column(2),
  column(8),
  column(2)
)

input_row <- fluidRow(
  column(2),
  column(8),
  column(2)
)

download_row <- fluidRow(
  column(2),
  column(8),
  column(2)
)



ui <- fluidPage(
  titlePanel("Simulator"),
  simulate_row,
  bigbox_row,
  input_row,
  download_row
)


# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)
