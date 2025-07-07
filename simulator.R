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
