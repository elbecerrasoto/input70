library(tidyverse)

# ---- globals

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

get_e <- function(state) {
  x <- ALL[[state]]$x
  E <- get_E(state)

  E / x
}

get_T <- function(state) {
  e <- get_e(state)
  L <- ALL[[state]]$L
  Tm <- diag(e) %*% L
  mask <- is.nan(Tm) | is.infinite(Tm) | is.na(Tm)
  Tm[mask] <- 0
  Tm
}

get_Tmultipliers <- function(state) {
  colSums(get_T(state))
}

# ---- Es and Ts for all

all_Ts <- map(names(ALL), get_T)
all_Ts <- all_Ts |> set_names(names(ALL))

for (state in names(ALL)) {
  ALL[[state]]$Tm <- all_Ts[[state]]
}

write_rds(ALL, "data/all2.Rds")
