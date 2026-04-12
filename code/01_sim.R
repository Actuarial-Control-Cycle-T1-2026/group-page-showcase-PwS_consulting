# -------------------------------------------------------------------------------------------------------------------
# 02_sim_bi.R
# Monte Carlo simulation for Business Interruption pricing
# -------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------
# SCENARIOS
# -------------------------------------------------------------------------------------------------------------------

best_scenario_bi <- list(
  name               = "Best Case (Smooth Operations)",
  lambda_mult        = rep(0.9, 10),
  sev_shift          = c(rep(log(0.95), 3), rep(log(0.9), 4), rep(log(0.85), 3)),
  shock_prob         = 0,
  shock_sev_mult     = 0
)

moderate_scenario_bi <- list(
  name               = "Base Case (Model Expectations)",
  lambda_mult        = rep(1.00, 10),
  sev_shift          = rep(0.00, 10),
  shock_prob         = 0,
  shock_sev_mult     = 0
)

worst_scenario_bi <- list(
  name               = "Worst Case (Correlated Deterioration)",
  lambda_mult        = rep(1.1, 10),
  sev_shift          = c(rep(log(1.12), 3), rep(log(1.15), 4), rep(log(1.2), 3)),
  shock_prob         = 0.10,
  shock_sev_mult     = 0.50
)

carrington_stress_bi <- list(
  name               = "Carrington-Scale Correlated Solar Event",
  lambda_mult        = rep(1.0, 1),
  sev_shift          = rep(0.0, 1),
  shock_prob         = 1,
  shock_sev_mult     = 1.50
)

# -------------------------------------------------------------------------------------------------------------------
# ECONOMIC SIMULATION
# -------------------------------------------------------------------------------------------------------------------

simulate_econ_ar1 <- function(years, sims = 10000,
                              infl_hist, disc_hist,
                              phi_inf = 0.5, phi_disc = 0.5,
                              infl_floor = 0.00,
                              disc_floor = 0.00,
                              seed = 123) {
  set.seed(seed)
  
  mu_inf   <- mean(infl_hist);  sd_inf  <- sd(infl_hist)
  mu_disc  <- mean(disc_hist);  sd_disc <- sd(disc_hist)
  
  sigma_inf_innov  <- sd_inf  * sqrt(1 - phi_inf^2)
  sigma_disc_innov <- sd_disc * sqrt(1 - phi_disc^2)
  
  rho <- cor(infl_hist, disc_hist)
  L   <- chol(matrix(c(1, rho, rho, 1), 2))
  
  infl <- matrix(NA_real_, sims, years)
  disc <- matrix(NA_real_, sims, years)
  
  Z           <- matrix(rnorm(sims * 2), sims, 2) %*% L
  infl[, 1]   <- mu_inf  + sd_inf  * Z[, 1]
  disc[, 1]   <- mu_disc + sd_disc * Z[, 2]
  
  if (years > 1) {
    for (yr in 2:years) {
      Z          <- matrix(rnorm(sims * 2), sims, 2) %*% L
      infl[, yr] <- mu_inf  + phi_inf  * (infl[, yr-1] - mu_inf)  + sigma_inf_innov  * Z[, 1]
      disc[, yr] <- mu_disc + phi_disc * (disc[, yr-1] - mu_disc) + sigma_disc_innov * Z[, 2]
    }
  }
  
  infl <- pmax(infl, infl_floor)
  disc <- pmax(disc, disc_floor)
  
  infl_cumul        <- infl
  disc_cumul        <- disc
  infl_cumul[, 1]   <- 1 + infl[, 1]
  disc_cumul[, 1]   <- 1 / (1 + disc[, 1])
  
  if (years > 1) {
    for (yr in 2:years) {
      infl_cumul[, yr] <- infl_cumul[, yr-1] * (1 + infl[, yr])
      disc_cumul[, yr] <- disc_cumul[, yr-1] / (1 + disc[, yr])
    }
  }
  
  list(infl_annual = infl, disc_annual = disc,
       infl_cumul  = infl_cumul, disc_cumul  = disc_cumul)
}


# -------------------------------------------------------------------------------------------------------------------
# YEAR-BY-YEAR PREMIUM VECTOR (Years 1-10)
# Premium = inflated base EL_net, loaded for expenses and profit
# -------------------------------------------------------------------------------------------------------------------

# Simulate AR(1) inflation paths first (reuse your simulate_econ_ar1 function)
econ_bi <- simulate_econ_ar1(
  years     = 10,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  seed      = 123
)


EL_net_sev <- function(mu, sigma, ded, lim) {
  lev_lnorm <- function(k, m, s) {
    term1 <- exp(m + (s^2)/2) * pnorm((log(k) - m - s^2) / s)
    term2 <- k * (1 - pnorm((log(k) - m) / s))
    return(term1 + term2)
  }
  exp_net <- lev_lnorm(lim, mu, sigma) - lev_lnorm(ded, mu, sigma)
  return(exp_net)
}

# Expense loading: front-loaded in Years 1-3, steps down thereafter
expense_loading_vec <- ifelse(1:10 <= 3, 0.25, 0.25 -(1:10 - 4)* 0.01)

profit_loading      <- 0.1

# Year-by-year expected premium vector (length 10)
# Stations grow via growth_model_bi(); EL_net_sev is constant (no scenario shift on base)
premium_vec <- sapply(1:10, function(yr) {
  
  # Total stations in year t (summed across all three systems)
  stations_yr <- sum(sapply(
    c("Helionis Cluster", "Bayesia System", "Oryn Delta"),
    function(sys) {
      system_base$unit_count_base[system_base$solar_system == sys] *
        growth_model_bi(sys, yr)
    }
  ))
  
  # Scale mu_nb (per-station frequency) to total stations,
  mean_infl_t <- if (yr == 1) 1 else mean(econ_bi$infl_cumul[, yr - 1])
  
  EL_net_yr <- (mu_nb * stations_yr) *
    EL_net_sev(mu_sev, sigma_sev,
               deductible_q10 * mean_infl_t,   
               limit_q90      * mean_infl_t)   #
  
  # Premium loaded for expenses and profit
  EL_net_yr / (1 - expense_loading_vec[yr] - profit_loading)
})

names(premium_vec) <- paste0(1:10)

# -------------------------------------------------------------------------------------------------------------------
# MAIN SIMULATION FUNCTION
# -------------------------------------------------------------------------------------------------------------------

run_mc_bi <- function(dt,
                      horizon,
                      sims,
                      infl_hist,
                      disc_hist,
                      scenario,
                      seed = 123) {
  
  # ── economic paths ───────────────────────────────────────────────────────────
  econ <- simulate_econ_ar1(
    years     = horizon,
    sims      = sims,
    infl_hist = infl_hist,
    disc_hist = disc_hist,
    seed      = seed
  )
  
  # ── matrix initialisations ───────────────────────────────────────────────────
  premium_mat      <- matrix(0, nrow = sims, ncol = horizon)
  loss_mat         <- matrix(0, nrow = sims, ncol = horizon)
  gross_loss_mat   <- matrix(0, nrow = sims, ncol = horizon)
  gross_loss_disc_mat   <- matrix(0, nrow = sims, ncol = horizon)
  loss_disc_mat    <- matrix(0, nrow = sims, ncol = horizon)
  profit_mat       <- matrix(0, nrow = sims, ncol = horizon)
  gross_profit_mat <- matrix(0, nrow = sims, ncol = horizon)
  freq_mat         <- matrix(0, nrow = sims, ncol = horizon)
  
  # per-simulation severity storage
  sev_list_raw   <- vector("list", sims)
  sev_list_net   <- vector("list", sims)
  sev_list_gross <- vector("list", sims)
  for (s in 1:sims) {
    sev_list_raw[[s]]   <- vector("list", horizon)
    sev_list_net[[s]]   <- vector("list", horizon)
    sev_list_gross[[s]] <- vector("list", horizon)
  }
  
  # ── main loop ────────────────────────────────────────────────────────────────
  for (yr in 1:horizon) {
    
    stations_current <- dt$total_stations[yr]
    stations_prev    <- if (yr == 1) stations_current else dt$total_stations[yr - 1]
    
    yr_idx       <- min(yr, length(scenario$lambda_mult))
    lam_mult_yr  <- scenario$lambda_mult[yr_idx]
    sev_shift_yr <- scenario$sev_shift[yr_idx]
    
    expense_loading <- ifelse(yr <= 3, 0.25, 0.25 - (yr - 4) * 0.01)
    
    for (s in 1:sims) {
      
      disc_s <- max(econ$disc_annual[s, yr], 0.001)
      infl_s <- econ$infl_cumul[s, yr]
      
      # premium
      premium_mat[s, yr] <- premium_vec[yr] *
        infl_s
      
      # ── frequency ────────────────────────────────────────────────────────────
      mean_claims_yr <- mu_nb * stations_current * lam_mult_yr
      
      n_claims <- rnbinom(
        n    = 1,
        size = r_nb * stations_current,
        mu   = mean_claims_yr
      )
      
      freq_mat[s, yr] <- n_claims
      
      total_net   <- 0
      total_gross <- 0
      
      sev_raw_vec   <- numeric(n_claims)
      sev_net_vec   <- numeric(n_claims)
      sev_gross_vec <- numeric(n_claims)
      
      # ── severity ─────────────────────────────────────────────────────────────
      if (n_claims > 0) {
        
        mu_eff <- mu_sev + sev_shift_yr
        
        sev_raw <- rlnorm(
          n       = n_claims,
          meanlog = mu_eff,
          sdlog   = sigma_sev
        ) * infl_s
        
        # correlated shock event
        if (scenario$shock_prob > 0 &&
            rbinom(1, 1, scenario$shock_prob) == 1) {
          sev_raw <- sev_raw * (1 + scenario$shock_sev_mult)
        }
        
        sev_net <- pmin(
          pmax(sev_raw - (deductible_q10 * infl_s), 0),
          (limit_q90 * infl_s) - (deductible_q10 * infl_s)
        )
        
        sev_raw_vec   <- sev_raw
        sev_net_vec   <- sev_net
        sev_gross_vec <- sev_raw
        
        total_net   <- sum(sev_net)
        total_gross <- sum(sev_raw)
      }
      
      sev_list_raw[[s]][[yr]]   <- sev_raw_vec
      sev_list_net[[s]][[yr]]   <- sev_net_vec
      sev_list_gross[[s]][[yr]] <- sev_gross_vec
      
      # ── discount + profit ─────────────────────────────────────────────────────
      disc_factor <- 1 / (1 + disc_s)^yr
      
      expense_loading <- ifelse(yr <= 3, 0.15, 0.15 - (yr - 4) * 0.01) 
      prem_s <- premium_mat[s, yr]
      expense_var <- prem_s * expense_loading 
      
      if (yr == 1) {
        fixed_expense <- 0.05 * prem_s
      } else {
        fixed_expense <- 0.01 * prem_s * infl_s 
      }
      expense <- expense_var + fixed_expense
      
      loss_mat[s, yr]         <- total_net
      gross_loss_mat[s, yr]   <- total_gross
      loss_disc_mat[s, yr]    <- total_net * disc_factor
      gross_loss_disc_mat[s, yr]    <- total_gross * disc_factor
      
      profit_mat[s, yr]       <- (prem_s - total_net   - expense) * disc_factor
      gross_profit_mat[s, yr] <- (prem_s - total_gross - expense) * disc_factor
    }
  }
  
  # ── results summary ──────────────────────────────────────────────────────────
  yearly <- purrr::map_dfr(1:horizon, function(yr) {
    losses  <- loss_mat[, yr]
    profits <- profit_mat[, yr]
    prems   <- premium_mat[, yr]
    gross_profits <- gross_profit_mat[,yr]
    q99     <- quantile(losses, 0.99)
    
    data.frame(
      Year          = yr,
      avg_premium   = round(mean(prems), 0),
      expected_loss = round(mean(losses), 0),
      loss_ratio    = round(mean(losses) / mean(prems), 3),
      variance_profit =sd(gross_profits),
      EV_profit    = round(mean(profits), 0),
      prob_loss     = round(mean(profits < 0), 3),
      VaR99         = round(q99, 0),
      TVaR99        = round(mean(losses[losses >= q99]), 0)
    )
  })
  
  summary <- data.frame(
    scenario      = scenario$name,
    horizon       = horizon,
    total_premium = round(sum(colMeans(premium_mat)), 0),
    total_loss    = round(sum(colMeans(loss_mat)), 0),
    total_profit  = round(sum(colMeans(profit_mat)), 0),
    proft_std = sqrt(var(loss_mat[, 1])),
    VaR99_yr1     = round(quantile(loss_mat[, 1], 0.99), 0),
    TVaR99_yr1    = round(mean(loss_mat[, 1][loss_mat[, 1] >= quantile(loss_mat[, 1], 0.99)]), 0)
  )
  
  list(
    summary          = summary,
    yearly           = yearly,
    loss_mat         = loss_mat,
    gross_loss_mat   = gross_loss_mat,
    loss_disc_mat = loss_disc_mat,
    gross_loss_disc_mat = gross_loss_disc_mat,
    profit_mat       = profit_mat,
    gross_profit_mat = gross_profit_mat,
    freq_mat         = freq_mat,
    premium_mat      = premium_mat,
    sev_list_raw     = sev_list_raw,
    sev_list_net     = sev_list_net
  )
}

# -------------------------------------------------------------------------------------------------------------------
# RUN SIMULATIONS
# -------------------------------------------------------------------------------------------------------------------

bi_base_1yr <- run_mc_bi(
  dt        = bi_dt,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = moderate_scenario_bi,
  seed      = 123
)

bi_base_10yr <- run_mc_bi(
  dt        = bi_dt,
  horizon   = 10,
  sims      = 100000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = moderate_scenario_bi,
  seed      = 123
)

bi_best <- run_mc_bi(
  dt        = bi_dt_best,
  horizon   = 10,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = best_scenario_bi,
  seed      = 123
)

bi_best_1yr <- run_mc_bi(
  dt        = bi_dt_best,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = best_scenario_bi,
  seed      = 123
)

bi_worst <- run_mc_bi(
  dt        = bi_dt_worst,
  horizon   = 10,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = worst_scenario_bi,
  seed      = 123
)

bi_worst_1yr <- run_mc_bi(
  dt        = bi_dt_worst,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = worst_scenario_bi,
  seed      = 123
)

bi_stress <- run_mc_bi(
  dt        = bi_dt,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = carrington_stress_bi,
  seed      = 123
)

# -------------------------------------------------------------------------------------------------------------------
# RESULTS
# -------------------------------------------------------------------------------------------------------------------

q99 <-quantile(bi_base_1yr$gross_loss_mat,0.99)
mean(bi_base_1yr$gross_loss_mat[bi_base_1yr$gross_loss_mat >= q99])


bi_base_1yr$summary
bi_base_10yr$summary
bi_base_10yr$yearly

bi_best$summary
bi_worst$summary
bi_stress$summary

combined <- bind_rows(
  bi_base_1yr$summary%>% mutate(scenario = "1-yr Base"),
  bi_base_10yr$summary %>% mutate(scenario = "10-yr Base"),
  bi_best_1yr$summary %>% mutate(scenario = "1-yr Best"),
  bi_best$summary %>% mutate(scenario = "10-yr Best"),
  bi_worst_1yr$summary %>% mutate(scenario = "1-yr Best"),
  bi_worst$summary %>% mutate(scenario = "10-yr Worst")
)

write.csv(combined, "yearly_results.csv", row.names = FALSE)
