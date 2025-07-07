library(tidyverse)
library(readxl)

x <- read_xlsx("PT_MIP_Nacional_Homologada.xlsx")
population <- read_xlsx("data/Poblacion_Edited.xlsx")
ALL <- read_rds("data/all.Rds")

population <- population |>
  mutate(
    state_key = names(ALL)
  ) |>
  relocate(state_key)

E_national <- x[1, 4:38] |> as.numeric()
total_working <- sum(E_national)

mexico_pop <- sum(population$total)
working_coef <- total_working / mexico_pop

elasticities <- E_national / total_working

get_state_pop <- function(state) {
  population |>
    filter(state_key == state) |>
    pull(total)
}

state_pop <- get_state_pop("sinaloa")

rest_working <- (mexico_pop - state_pop) * working_coef
state_working <- state_pop * working_coef


E_rest <- rest_working * elasticities
E_state <- state_working * elasticities
