inflation_path <- "C:/Users/pamel/Downloads/srcsc-2026-interest-and-inflation.xlsx"
df_rates <- as.data.frame(read_excel(inflation_path, skip = 2))

inf_series <- df_rates$Inflation
rate_series <- df_rates$`1-Year Risk Free Annual Spot Rate`


simulate_econ_ar1 <- function(years, sims = 10000,
                              infl_hist, disc_hist,
                              phi_inf = 0.5, phi_disc = 0.5,
                              infl_floor = 0.00,      # no negative inflation
                              disc_floor = 0.000,     # allow near-zero rates
                              seed = 123) {
  
  set.seed(seed)
  
  # Historical means and sds
  mu_inf  <- mean(infl_hist)
  mu_disc <- mean(disc_hist)
  sd_inf  <- sd(infl_hist)
  sd_disc <- sd(disc_hist)
  
  # Innovation sds for AR(1)
  sigma_inf_innov  <- sd_inf  * sqrt(1 - phi_inf^2)
  sigma_disc_innov <- sd_disc * sqrt(1 - phi_disc^2)
  
  # Correlation between inflation and discount rate shocks
  rho <- cor(infl_hist, disc_hist)
  L   <- chol(matrix(c(1, rho, rho, 1), 2))
  
  infl <- matrix(NA_real_, sims, years)
  disc <- matrix(NA_real_, sims, years)
  
  # Initial year: draw from correlated normals around historical means
  Z <- matrix(rnorm(sims * 2), sims, 2) %*% L
  infl[, 1] <- mu_inf  + sd_inf  * Z[, 1]
  disc[, 1] <- mu_disc + sd_disc * Z[, 2]
  
  # AR(1) evolution
  if (years > 1) {
    for (yr in 2:years) {
      Z <- matrix(rnorm(sims * 2), sims, 2) %*% L
      
      infl[, yr] <- mu_inf  + phi_inf  * (infl[, yr - 1] - mu_inf)  + sigma_inf_innov  * Z[, 1]
      disc[, yr] <- mu_disc + phi_disc * (disc[, yr - 1] - mu_disc) + sigma_disc_innov * Z[, 2]
    }
  }
  
  # Enforce non-negative (or floored) inflation and discount rates
  infl <- pmax(infl, infl_floor)
  disc <- pmax(disc, disc_floor)
  
  # Build cumulative inflation and discount factors
  infl_cumul <- infl
  disc_cumul <- disc
  
  infl_cumul[, 1] <- 1 + infl[, 1]
  disc_cumul[, 1] <- 1 / (1 + disc[, 1])
  
  if (years > 1) {
    for (yr in 2:years) {
      infl_cumul[, yr] <- infl_cumul[, yr - 1] * (1 + infl[, yr])
      disc_cumul[, yr] <- disc_cumul[, yr - 1] / (1 + disc[, yr])
    }
  }
  
  list(
    infl_annual = infl,
    disc_annual = disc,
    infl_cumul  = infl_cumul,
    disc_cumul  = disc_cumul
  )
}

