library(purrr)
library(stringr)

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

custom_numeric_input <- function(id, label, value = 0) {
  numericInput(id, label, value)
}

inputThirtyFiveState <- map2(
  str_c("s", 1:35),
  str_c("State: ", 1:35),
  custom_numeric_input
)

inputThirtyFiveRest <- map2(
  str_c("r", 1:35),
  str_c("Outer: ", 1:35),
  custom_numeric_input
)

inputRow <- fluidRow(
  column(2, inputThirtyFiveState),
  column(8),
  column(2, inputThirtyFiveRest),
)

downloadRow <- fluidRow(
  column(2),
  column(8),
  column(2)
)
