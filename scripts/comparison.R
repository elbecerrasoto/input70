# Comparison
# between IMSS
# and Elasticities

library(tidyverse)
source("get_T.R")

STATE <- "sinaloa"

imss <- read_csv("data/EmpleoFinalSinaloa_IMSS2.csv")
E_imss_raw <- imss[1, ] |> as.numeric()
elasticities_imss <- E_imss_raw / sum(E_imss_raw)

state_pop <- get_state_pop(STATE)

E_imss_elas <- state_pop * WORKING_COEF * elasticities_imss

sector_short <- employment$sector |>
  str_remove(".*?_(?=[a-z])")

E_elasticities <- get_E(STATE)[1:35]

elasnat_rimss <-
  (abs(E_elasticities - E_imss_raw) / 1000) |>
  round(2) |>
  set_names(sector_short)

elastnat_eimss <-
  (abs(E_elasticities - E_imss_elas) / 1000) |>
  round(2) |>
  set_names(sector_short)

eimss_rimss <-
  (abs(E_imss_elas - E_imss_raw) / 1000) |>
  round(2) |>
  set_names(sector_short)

mean(elasnat_rimss^2)
mean(elastnat_eimss^2)
mean(eimss_rimss^2)

sinaloa_errors <-
  tibble(
    imss_raw = E_imss_raw,
    imss_elast = E_imss_elas,
    national_elast = E_elasticities,
    sector = sector_short,
    error1000s_natVimssRaw = elasnat_rimss,
    error1000s_natVimssElast = elastnat_eimss,
    error1000s_imssElastVimssRaw = eimss_rimss
  )

sinaloa_errors |>
  writexl::write_xlsx("sinaloa_errors.xlsx")


sinaloa_errors |>
  select(-error1000s_imssElastVimssRaw, -error1000s_natVimssRaw, -imss_raw) |>
  mutate(
    imss_elast = as.integer(imss_elast),
    national_elast = as.integer(national_elast)
  ) |>
  writexl::write_xlsx("sinaloa_errors_human.xlsx")


best_guess <- ceiling((E_elasticities + E_imss_elas) / 2)
