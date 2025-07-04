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
  str_c("s", 1:32),
  str_c("State S ", 1:32),
  custom_numeric_input
)
inputThirtyFiveRest <- custom_numeric_input("r1", "Rest pais apaiaitaiets aestes 1")

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
