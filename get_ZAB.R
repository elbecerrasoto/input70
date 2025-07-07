library(tidyverse)
source("get_ZAB_helper.R")

TOLERANCE <- 1e-2

N_REGION <- 35
N_OUTER <- 35
N_SECTORS <- N_REGION + N_OUTER

get_ZAB_LG_fx <- function(Z_aug) {
  Z <- get_Z(Z_aug)
  f <- get_f(Z_aug)
  x <- get_x(Z_aug)

  A <- get_A(Z, x)
  B <- get_B(Z, x)

  L <- get_L(A)
  G <- get_G(B)

  list(Z = Z, A = A, B = B, L = L, G = G, f = f, x = x)
}

get_M1_M2_M3 <- function(A) {
  check_square(A)

  n <- ncol(A)
  r <- N_REGION
  s <- r + 1

  # ------ Regionalize

  Arr <- A[1:r, 1:r]
  Ars <- A[1:r, s:n]

  Ass <- A[s:n, s:n]
  Asr <- A[s:n, 1:r]

  Irr <- diag(N_REGION)
  Iss <- diag(N_OUTER)

  Ors <- matrix(0, nrow = N_REGION, ncol = N_OUTER)
  Osr <- matrix(0, nrow = N_OUTER, ncol = N_REGION)

  Lrr <- solve(Irr - Arr)
  Lss <- solve(Iss - Ass)

  # Spillover
  Srs <- Lrr %*% Ars
  Ssr <- Lss %*% Asr

  # Feed-back
  Frr <- solve(Irr - Srs %*% Ssr)
  Fss <- solve(Iss - Ssr %*% Srs)

  M1_rr_sr_col <- rbind(Lrr, Osr)
  M1_rs_ss_col <- rbind(Ors, Lss)
  M1 <- cbind(M1_rr_sr_col, M1_rs_ss_col)

  M2_rr_sr_col <- rbind(Irr, Ssr)
  M2_rs_ss_col <- rbind(Srs, Iss)
  M2 <- cbind(M2_rr_sr_col, M2_rs_ss_col)

  M3_rr_sr_col <- rbind(Frr, Osr)
  M3_rs_ss_col <- rbind(Ors, Fss)
  M3 <- cbind(M3_rr_sr_col, M3_rs_ss_col)

  list(M1 = M1, M2 = M2, M3 = M3)
}
