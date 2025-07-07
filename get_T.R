library(tidyverse)

# ---- GLOBALS

population <- read_tsv("data/mexico_population.tsv")
employment <- read_tsv("data/mexico_employment_by_sector.tsv")

E_national <- employment$employees
total_working <- sum(E_national)

ALL <- read_rds("data/all.Rds")

ELASTICITIES <- E_national / total_working
MEXICO_POP <- sum(population$total)
WORKING_COEF <- total_working / MEXICO_POP

# ---- helpers

get_state_pop <- function(state) {
  population |>
    filter(state_key == state) |>
    pull(total)
}

get_E <- function(state) {
  state_pop <- get_state_pop(state)

  rest_working <- (MEXICO_POP - state_pop) * WORKING_COEF
  state_working <- state_pop * WORKING_COEF

  E_state <- state_working * ELASTICITIES
  E_rest <- rest_working * ELASTICITIES

  c(E_state, E_rest)
}

# ---- Es and Ts for all

# ---- Sinaloa
state <- "sinaloa"
ALL[[state]]

E_sinaloa <- get_E(state)
E_sinaloa
