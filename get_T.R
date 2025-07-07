library(tidyverse)
library(readxl)
library(janitor)

x <- read_xlsx("PT_MIP_Nacional_Homologada.xlsx")
population <- read_xlsx("data/Poblacion_Edited.xlsx")
states_keys <- read_csv("data/states.csv")
ALL <- read_rds("data/all.Rds")


population <- population |>
  mutate(
    state_key = names(ALL)
  ) |>
  relocate(state_key)

population |> view()

names(x) |> make_clean_names()
