library(tidyverse)
library(glue)
source("get_ZAB.R")

STATES <- read_rds("data/mips_br.Rds")

get_everything <- function(Z_aug) {
  ZABs <- get_ZAB_LG_fx(Z_aug)
  Ms <- get_M1_M2_M3(ZABs$A)
  c(ZABs, Ms)
}

all_ZABs_Ms <- map(STATES, get_everything)

get_multipliers <- function(state) {
  current <- all_ZABs_Ms[[state]]

  attach(current)

  output_multipliers <- colSums(L)
  input_multipliers <- rowSums(G)

  are_not_less_than_1 <- all(output_multipliers >= 1)
  stopifnot("Multipliers are less than 1." = are_not_less_than_1)

  BL <- get_linkage(L, backward = TRUE)
  FL <- get_linkage(G, backward = FALSE)

  sectors <- colnames(Z)

  multipliers <- tibble(
    output_multiplier = output_multipliers,
    input_multiplier = input_multipliers,
  )

  code <- sectors |>
    str_extract_all("\\d+") |>
    map_chr(str_flatten, collapse = "-")

  sector <- sectors |>
    str_extract("\\d+.*?$") |>
    str_remove_all("\\d+_") |>
    str_sub(start = 1, end = 32)

  region <- sectors |>
    str_remove("_\\d+.*$") |>
    str_replace("(resto_del_pais)", glue("{state}_\\1"))

  # output multipliers
  multipliers <- multipliers |>
    mutate(
      region = region,
      code = code,
      sector = sector
    )

  # ---- expand multipliers to other summaries

  link_class <- vector(mode = "character", length = length(BL))

  link_class[BL < 1 & FL < 1] <- "independent"
  link_class[BL < 1 & FL >= 1] <- "demand_dependent"
  link_class[BL >= 1 & FL < 1] <- "supply_dependent"
  link_class[BL >= 1 & FL >= 1] <- "dependent"

  multipliers <- multipliers |>
    mutate(
      link_backward = BL,
      link_forward = FL,
      link_class = link_class
    )

  I <- diag(N_SECTORS)

  M1a <- M1 - I
  M2a <- (M2 - I) %*% M1
  M3a <- (M3 - I) %*% M2 %*% M1

  intra <- M1a |> colSums()
  spillover <- M2a |> colSums()
  feedback <- M3a |> colSums()


  multipliers <- multipliers |>
    mutate(
      intraregional = intra,
      spillover = spillover,
      feedback = feedback,
      mip = state,
      raw_name = sectors
    )

  detach(current)
  multipliers |>
    select(
      output_multiplier,
      sector,
      region,
      link_class,
      intraregional,
      spillover,
      feedback,
      code,
      input_multiplier,
      link_backward,
      link_forward,
      mip,
      raw_name
    )
}


all_states_multipliers <- imap(all_ZABs_Ms, \(data, state) get_multipliers(state))

multiplers <- bind_rows(all_states_multipliers)

all_states_multipliers
all_ZABs_Ms

all_ZAB_multipliers <- map2(all_states_multipliers, all_ZABs_Ms, c)

multiplers |>
  writexl::write_xlsx("multipliers.xlsx")

multiplers |>
  write_tsv("multipliers.tsv")

write_rds(all_ZAB_multipliers, "data/all.Rds")
