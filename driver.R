#!/usr/bin/Rscript

library(tidyverse)
source("simulator.R")

N_REGION <- 35
N_OUTER <- 35

N_SECTORS <- N_REGION + N_OUTER

STATES <- read_rds("data/mips_br.Rds")

results <- get_ZABLGfx_multipliers(STATES[["sinaloa"]], N_SECTORS)
A <- results$A |> tib2mat()
L <- results$L |> tib2mat()

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

# FeedBack
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

L321 <- M3 %*% M2 %*% M1

Ls_equal <- near(L321, L, tol = 1e4) |> all()
stopifnot("Mutiplicative Decomposition Failed" = Ls_equal)

# ------ Additive descomposition

I <- diag(N_SECTORS)

I <- diag(N_SECTORS)
Ladd <- I + (M1 - I) + (M2 - I) %*% M1 + (M3 - I) %*% M2 %*% M1

M1a <- M1 - I
M2a <- (M2 - I) %*% M1
M3a <- (M3 - I) %*% M2 %*% M1

# 379 Miller & Blair
# Initial injections
# Add net intraregional effects
# Add the net interregional spillover effects
# Add the net interregional feedback effects
# x = Mf = I %*% f + M1a %*% f + M2a %*% f + M3a %*% f

intrar_e <- M1a |> colSums()
spillover_e <- M2a |> colSums()
feedback_e <- M3a |> colSums()
