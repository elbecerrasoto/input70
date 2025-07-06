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

check_square <- function(M) {
  if (nrow(M) != ncol(M)) stop("Matrix is not square.")
}

get_Z <- function(Z_aug) {
  # tib -> mat
  Z_aug <- Z_aug |> select(where(is.numeric))
  Z <- Z_aug[1:N_SECTORS, 1:N_SECTORS]
  tib2mat(Z)
}

get_x <- function(Z_aug) {
  # tib -> double
  Z_aug <- Z_aug |> select(where(is.numeric))
  x_row <- rowSums(Z_aug[1:N_SECTORS, ])
  x_col <- colSums(Z_aug[, 1:N_SECTORS])

  are_xs_equal <- all(near(x_row, x_col, TOLERANCE))
  stopifnot("Row and Col totals do NOT match." = are_xs_equal)

  return(x_row)
}

get_f <- function(Z_aug) {
  # tib -> double
  Z_aug <- Z_aug |> select(where(is.numeric))
  Z_aug[1:N_SECTORS, -1:-N_SECTORS] |>
    rowSums()
}

rows_or_cols <- function(M, byrow = TRUE) {
  if (byrow) {
    out <- map(1:nrow(M), \(i) as.numeric(M[i, ]))
  } else {
    out <- map(1:ncol(M), \(i) as.numeric(M[, i]))
  }
  out
}

normalize <- function(M, x, byrow = TRUE) {
  # mat, double -> mat
  rocs <- rows_or_cols(M, byrow)
  f <- function(roc, i) {
    AVOID_UNDEF <- 1
    if (x[i] == 0) {
      out <- roc / AVOID_UNDEF
    } else {
      out <- roc / x[i]
    }
    out
  }
  imap(rocs, f) |>
    unlist() |>
    matrix(
      nrow = nrow(M),
      ncol = ncol(M),
      byrow = byrow
    )
}

get_A <- function(Z, x) {
  normalize(Z, x, byrow = FALSE)
}

get_L <- function(A) {
  check_square(A)
  I <- diag(ncol(A))
  solve(I - A)
}

get_B <- function(Z, x) normalize(Z, x, byrow = TRUE)

get_G <- function(B) get_L(B)

get_linkage <- function(M) {
  n_sectors <- ncol(M)
  multipliers <- M |> colSums()

  are_not_less_than_1 <- all(output_multipliers >= 1)
  stopifnot("Multipliers are less than 1." = are_not_less_than_1)

  multipliers_mean <- multipliers |> sum() / n_sectors
  multipliers / multipliers_mean
}
