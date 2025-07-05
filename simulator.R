#!/usr/bin/Rscript
library(tidyverse)
library(glue)

tib2mat <- function(tib, drop_names = FALSE) {
  mat <- tib |>
    select(where(is.numeric)) |>
    as.matrix()
  if (drop_names) {
    colnames(mat) <- NULL
    rownames(mat) <- NULL
  }
  mat
}

normalize_sector <- function(sector_inputs, sector, x) {
  AVOID_UNDEF <- 1
  xj <- x[sector]

  if (xj == 0) {
    return(sector_inputs / AVOID_UNDEF)
  } else {
    return(sector_inputs / xj)
  }
}

get_A <- function(Z, x) {
  # fails if Z and x names are not equal
  Z |>
    imap(normalize_sector, x = x) |>
    as_tibble()
}

get_L <- function(A) {
  is_square <- (nrow(A) == ncol(A))
  if (!is_square) {
    stop("Input requirement matrix must be square.")
  }
  I <- diag(ncol(A))
  solve(I - A) |>
    as_tibble() |>
    set_names(names(A))
}

get_B <- function(Z, x) {
  Zt <- Z |>
    tib2mat() |>
    t() |>
    as_tibble() |>
    set_names(names(Z))
  get_A(Zt, x) |>
    tib2mat() |>
    t() |>
    as_tibble() |>
    set_names(names(Z))
}

get_G <- function(B) {
  get_L(B)
}


get_linkage <- function(L) {
  n_sectors <- ncol(L)
  multipliers <- L |> colSums()
  multipliers_mean <- multipliers |> sum() / n_sectors
  multipliers / multipliers_mean
}


get_ZABLGfx_multipliers <- function(Z_aug, n_sectors) {
  # ---- globals

  TOLERANCE <- 1e-2
  N_SECTORS <- n_sectors
  OUT_NAMES <- c("Z", "A", "L", "f", "x", "multipliers")
  OUT <- vector(mode = "list", length = length(OUT_NAMES)) |> set_names(OUT_NAMES)

  # ---- get Z, A, B, L, G, f, x

  Z_aug <- Z_aug |> select(where(is.numeric))
  Z <- Z_aug[1:N_SECTORS, 1:N_SECTORS]
  sector_names <- names(Z)
  Zm <- tib2mat(Z, drop_names = TRUE)

  x_row <- rowSums(Z_aug[1:N_SECTORS, ])
  x_col <- colSums(Z_aug[, 1:N_SECTORS])

  are_xs_equal <- all(near(x_row, x_col, TOLERANCE))
  stopifnot("Row and Col totals do NOT match." = are_xs_equal)

  x <- x_row |> set_names(names(Z))

  # final demand
  f <- Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
    rowSums() |>
    set_names(names(Z))

  A <- get_A(Z, x)
  Am <- tib2mat(A, drop_names = TRUE)

  B <- get_B(Z, x)
  Bm <- tib2mat(B, drop_names = TRUE)

  L <- get_L(A)
  Lm <- tib2mat(L, drop_names = TRUE)

  G <- get_G(B)
  Gm <- tib2mat(G, drop_names = TRUE)

  # ---- get multipliers

  output_multipliers <- colSums(L)
  input_multipliers <- colSums(G)

  BL <- get_linkage(L) |> set_names(names(Z))
  FL <- get_linkage(G) |> set_names(names(Z))

  are_not_less_than_1 <- all(output_multipliers >= 1)
  stopifnot("Multipliers are less than 1." = are_not_less_than_1)

  multipliers <- tibble(
    output_multiplier = output_multipliers,
    input_multiplier = input_multipliers,
    sector_raw = names(output_multipliers)
  )

  naics <- multipliers$sector_raw |>
    str_extract_all("\\d+") |>
    map_chr(str_flatten, collapse = "-")

  sector <- multipliers$sector_raw |>
    str_extract("\\d+.*?$") |>
    str_remove_all("\\d+_")

  region <- multipliers$sector_raw |>
    str_remove("_\\d+.*$")

  # output multipliers
  multipliers <- multipliers |>
    mutate(
      region = region,
      code = naics,
      sector = sector
    ) |>
    select(-sector_raw)

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
    ) |>
    select(input_multiplier, output_multiplier, link_class, sector, code, everything())

  OUT[["Z"]] <- Z
  OUT[["A"]] <- A
  OUT[["B"]] <- A
  OUT[["L"]] <- L
  OUT[["G"]] <- G
  OUT[["f"]] <- f
  OUT[["x"]] <- x
  OUT[["multipliers"]] <- multipliers

  OUT
}

simulate_demand_shocks <-
  function(shocks, L, f_old, x_old,
           shocks_are_multipliers = FALSE,
           shocks_are_total_demand = FALSE) {
    Lm <- L |> tib2mat()

    if (shocks_are_multipliers) {
      f_new <- shocks * f_old
    } else if (shocks_are_total_demand) {
      f_new <- shocks
    } else {
      f_new <- shocks + f_old
    }

    x_new <- Lm %*% f_new |> as.numeric()
    delta <- x_new - x_old
    delta_rel <-
      if_else(x_old != 0, (delta + x_old) / x_old, (delta + x_old) / 1)

    tibble(f_old, x_old, f_new, x_new, delta, delta_rel)
  }
