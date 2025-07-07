library(tidyverse)
library(readxl)
library(janitor)

x <- read_xlsx("PT_MIP_Nacional_Homologada.xlsx")
population <- read_xlsx("data/Poblacion_Edited.xlsx")
ALL <- read_rds("data/all.Rds")

population <- population |>
  mutate(
    state_key = names(ALL)
  ) |>
  relocate(state_key)

E_national <- x[1, 4:38] |> as.numeric()

sectors <- names(x) |> make_clean_names()
sectors <- sectors[4:38]

E_national <- x[1, 4:38] |> as.numeric()

mexico_pop <- sum(population$total)
working_pop <- sum(E_national) / mexico_pop
