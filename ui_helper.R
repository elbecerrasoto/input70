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

inputThirtyFiveState <- numericInput("s1", "State 1", value = 0.0)
inputThirtyFiveRest <- numericInput("r1", "Rest 1", value = 0.0)

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

x <- expression(summary(iris))
eval(x)
