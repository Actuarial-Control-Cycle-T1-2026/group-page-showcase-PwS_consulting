# -------------------------------------------------------------------------------------------------------------------
# SCENARIO DEFINITIONS
# Drivers that can shift equipment risk profile year-by-year
# -------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------
# SCENARIO DEFINITIONS AND NARRATIVE
# -------------------------------------------------------------------------------------------------------------------

best_scenario <- list(
  maintenance_shift  = c(1.05, 1.05, 1.07, 1.08, 1.09, 1.10, 1.11, 1.12, 1.13, 1.15),
  usage_shift         = c(0.90, 0.9, 0.9, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85),
  shock_prob         = 0,
  shock_sev_mult     = 0,
  admin_expense_rate = rep(0.005, 10)
  
)

moderate_scenario <- list(
  maintenance_shift  = rep(1.00, 10),
  usage_shift        = rep(1.00, 10),
  shock_prob         = 0,
  shock_sev_mult     = 0,
  admin_expense_rate = rep(0.02, 10)
)

worst_scenario <- list(
  maintenance_shift  = c(1.00, 0.98, 0.96, 0.94, 0.92, 0.89, 0.90, 0.89, 0.88, 0.85),
  usage_shift        = c(1.00, 1.02, 1.04, 1.06, 1.08, 1.12, 1.10, 1.11, 1.12, 1.14),
  shock_prob         = 0.10,
  shock_sev_mult     = 0.50,
  admin_expense_rate = rep(0.03, 10)
)

# STRESS TEST — "Carrington-Scale Solar Storm" (1-in-100 year event)
solar_storm_stress <- list(
  maintenance_shift  = 0.85, 
  usage_shift        = 1.5,
  shock_prob         = 1, 
  
  # Equipment severity multipliers by risk tier 
  equip_sev_tier <- c(
    "Quantum Bore"       = 1.80, 
    "FexStram Carrier"   = 1.80, 
    "Ion Pulverizer"     = 1.40, 
    "Graviton Extractor" = 1.40,  
    "ReglAggregators"    = 1.10,  
    "Flux Rider"         = 1.10  
  ),
  # System-specific frequency multipliers — reflects differential shielding / exposure
  system_freq_mult = c(
    "Bayesia System"    = 3.5,  # Highest — binary star EMP spikes, thin magnetosphere
    "Helionis Cluster"  = 2.5,  # Moderate — stable star but asteroid relay disruption delays response
    "Oryn Delta"        = 2.0   # Lower stellar exposure but worst comms delays post-storm
  ),
  shock_sev_mult     = 1.40,
  equip_sev_mult     = equip_sev_tier,
  admin_expense_rate = 0.25
)

# -------------------------------------------------------------------------------------------------------------------
# SCENARIO DRIVER APPLICATION
# Shifts maintenance_int and usage_int within their valid ranges
# -------------------------------------------------------------------------------------------------------------------

apply_ef_scenario <- function(dty, scenario_year) {
  
  dty_adj <- copy(as.data.table(dty))
  # Maintenance interval (100 – 5000 Earth hours)
  if (!is.null(dty_adj$maintenance_int)) {
    dty_adj[, maintenance_int := pmin(5000, pmax(100,
                                                 maintenance_int * scenario_year$maintenance_shift))]
  }
  # Usage intensity (0 – 24 Earth hours/day)
  if (!is.null(dty_adj$usage_int)) {
    dty_adj[, usage_int := pmin(24, pmax(0,
                                         usage_int * scenario_year$usage_shift))]
  }
  
  return(dty_adj)
}

# -------------------------------------------------------------------------------------------------------------------
# ECONOMIC SCENARIO GENERATOR
# -------------------------------------------------------------------------------------------------------------------

#Stochastic projection of inflation/discount rates
inf_series <- df_rates$inflation
rate_series <- df_rates$x1_year_risk_free_annual_spot_rate

#ARIMA model for inflation and discount rates - ARIMA model
fit_inf  <- auto.arima(inf_series)
fit_disc <- auto.arima(rate_series)

simulate_econ_ar1 <- function(years, sims = 10000,
                              infl_hist, disc_hist,
                              phi_inf = 0.3, phi_disc = 0.3,
                              infl_floor = 0.00,
                              disc_floor = 0.000,
                              seed = 123) {
  
  set.seed(seed)
  
  mu_inf  <- mean(infl_hist);  mu_disc <- mean(disc_hist)
  sd_inf  <- sd(infl_hist);    sd_disc <- sd(disc_hist)
  
  sigma_inf_innov  <- sd_inf  * sqrt(1 - phi_inf^2)
  sigma_disc_innov <- sd_disc * sqrt(1 - phi_disc^2)
  
  rho <- cor(infl_hist, disc_hist)
  L   <- chol(matrix(c(1, rho, rho, 1), 2))
  
  infl <- matrix(NA_real_, sims, years)
  disc <- matrix(NA_real_, sims, years)
  
  Z <- matrix(rnorm(sims * 2), sims, 2) %*% L
  infl[, 1] <- mu_inf  + sd_inf  * Z[, 1]
  disc[, 1] <- mu_disc + sd_disc * Z[, 2]
  
  if (years > 1) {
    for (yr in 2:years) {
      Z <- matrix(rnorm(sims * 2), sims, 2) %*% L
      infl[, yr] <- mu_inf  + phi_inf  * (infl[, yr - 1] - mu_inf)  + sigma_inf_innov  * Z[, 1]
      disc[, yr] <- mu_disc + phi_disc * (disc[, yr - 1] - mu_disc) + sigma_disc_innov * Z[, 2]
    }
  }
  
  infl <- pmax(infl, infl_floor)
  disc <- pmax(disc, disc_floor)
  
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


# -------------------------------------------------------------------------------------------------------------------
# ESG ADOPTION STATE
# -------------------------------------------------------------------------------------------------------------------

build_adoption_state <- function(dt,
                                 horizon        = 10,
                                 adoption_rate  = 0.60,
                                 annual_ramp    = 0.07,
                                 seed           = 42) {
  set.seed(seed)
  dt <- as.data.table(dt)
  
  # Work only on Oryn Delta existing cohorts at Year 1 (base rows)
  oryn_base <- dt[solar_system == "Oryn Delta" & year == 1 & cohort == "existing"]
  oryn_base[, row_id := .I]
  
  n_rows <- nrow(oryn_base)
  
  # adopted flag + years_adopted counter
  adopted      <- rep(FALSE, n_rows)
  years_adopted <- rep(0L,   n_rows)
  
  # Storage: year x row snapshot
  state_list <- vector("list", horizon)
  
  for (yr in seq_len(horizon)) {
    
    # Units not yet adopted get a 60% independent draw each year
    not_yet <- !adopted
    new_adopters <- not_yet & (runif(n_rows) < adoption_rate)
    adopted[new_adopters] <- TRUE
    
    # Increment years_adopted for all adopted units
    years_adopted[adopted] <- years_adopted[adopted] + 1L
    
    # Current maintenance intensity for this year
    # Additive accumulation: baseline * (1 + 0.07 * years_adopted)
    current_int <- oryn_base$maintenance_int * (1 + annual_ramp * years_adopted)
    
    # Cap at target (stop accumulating once target reached)
    current_int <- pmin(current_int, oryn_base$target_maintenance_int)
    
    # Eligibility: adopted AND at/above 150% target
    eligible_flag <- adopted & (current_int >= oryn_base$target_maintenance_int)
    
    state_list[[yr]] <- data.table(
      year              = yr,
      row_id            = oryn_base$row_id,
      equipment_type    = oryn_base$equipment_type,
      age_band          = oryn_base$age_band,
      unit_count        = oryn_base$unit_count,
      adopted           = adopted,
      years_adopted     = years_adopted,
      maintenance_int   = current_int,
      target_int        = oryn_base$target_maintenance_int,
      eligible          = eligible_flag
    )
  }
  
  rbindlist(state_list)
}


# -------------------------------------------------------------------------------------------------------------------
# MAIN MONTE CARLO FUNCTION
# -------------------------------------------------------------------------------------------------------------------

run_mc_ef <- function(dt,                      
                      horizon,                 
                      sims           = 10000,
                      infl_hist,
                      disc_hist,
                      phi_inf        = 0.3,
                      phi_disc       = 0.3,
                      scenario       = NULL, 
                      scenario_name  = NULL,
                      adoption_state = NULL,   # output of build_adoption_state(); NULL = no ESG discount
                      esg_discount   = 0.05,   # 5% premium discount for eligible Oryn Delta units
                      seed           = 123) {
  
  set.seed(seed)
  dt <- as.data.table(dt)
  
  # Default scenario = no shifts
  # Default scenario = no shifts
  if (is.null(scenario)) {
    scenario <- list(
      maintenance_shift = rep(1, horizon),
      usage_shift       = rep(1, horizon),
      shock_prob        = 0,
      shock_sev_mult    = 0,
      admin_expense_rate = rep(0, horizon)
    )
  }
  
  # Restrict to projection horizon and split by year
  dt_h     <- dt[year <= horizon]
  dt_split <- split(dt_h, by = "year", keep.by = FALSE)
  
  # Economic paths
  econ <- simulate_econ_ar1(
    years     = horizon,
    sims      = sims,
    infl_hist = infl_hist,
    disc_hist = disc_hist,
    phi_inf   = phi_inf,
    phi_disc  = phi_disc,
    seed      = seed
  )
  
  # Extract model parameter
  sigma_log <- sqrt(summary(final_sev)$dispersion)
  
  # -----------------------------------------------------------------
  # BASE PREMIUM — computed on Year 1 data, no scenario applied
  # -----------------------------------------------------------------
  dty1 <- dt_split[["1"]]
  
  if (!is.null(dty1) && nrow(dty1) > 0) {
    
    freq_exp <- predict(freq_final,
                        newdata = mutate(dty1, solar_system = solar_system_proxy),
                        type    = "response") * dty1$risk_scalar
    
    mu_sev   <- predict(final_sev,
                        newdata = mutate(dty1, solar_system = solar_system_proxy),
                        type    = "link")
    
    sev_exp  <- exp(mu_sev + 1/2 * sigma_log^2)
    
    EL_gross <- freq_exp * sev_exp * dty1$unit_count
    expense_loading <- 0.25
    profit_margin   <- 0.08
    
    premium_j    <- EL_gross / (1 - expense_loading - profit_margin)
    base_premium <- sum(premium_j)
    
  } else {
    base_premium <- 0
  }
  
  # -----------------------------------------------------------------
  # STORAGE
  # -----------------------------------------------------------------
  profit_mat    <- matrix(0, sims, horizon)
  gross_profit_mat    <- matrix(0, sims, horizon)
  gross_loss_mat      <- matrix(0, sims, horizon)
  loss_mat      <- matrix(0, sims, horizon)
  loss_disc_mat <- matrix(0, sims, horizon)
  premium_mat   <- matrix(0, sims, horizon)
  
  # -----------------------------------------------------------------
  # YEAR LOOP
  # -----------------------------------------------------------------
  for (yr in 1:horizon) {
    
    dty <- dt_split[[as.character(yr)]]
    if (is.null(dty) || nrow(dty) == 0) next
    
    # ── ESG maintenance ramp (adoption-state driven) ────────────────────────
    # If adoption_state is supplied, use per-cohort stochastic adoption tracking.
    # Otherwise fall back to no ESG adjustment (adoption_state = NULL).
    if (!is.null(adoption_state)) {
      state_yr <- adoption_state[year == yr]
      
      # Apply scenario maintenance shift on top of adoption ramp to test eligibility
      scen_shift_yr <- scenario$maintenance_shift[yr]
      state_yr[, maint_scenario := pmin(
        maintenance_int * scen_shift_yr,
        target_int
      )]
      state_yr[, eligible_final := adopted & (maint_scenario >= target_int)]
      
      elig_lookup <- state_yr[, .(equipment_type, age_band, eligible_final)]
      dty <- merge(dty, elig_lookup,
                   by = c("equipment_type", "age_band"),
                   all.x = TRUE)
      dty[is.na(eligible_final), eligible_final := FALSE]
      setnames(dty, "eligible_final", "premium_eligible_flag")
      
    } else {
      dty[, premium_eligible_flag := FALSE]
    }
    
    # Apply scenario drivers for this year
    scen_yr <- list(
      maintenance_shift = scenario$maintenance_shift[yr],
      usage_shift       = scenario$usage_shift[yr]
    )
    
    # ── Apply adoption-state maintenance improvements to dty ─────────────────
    if (!is.null(adoption_state)) {
      state_yr <- adoption_state[year == yr]
      scen_shift_yr <- scenario$maintenance_shift[yr]
      
      # Build lookup: equipment_type x age_band → improved maintenance_int
      maint_lookup <- state_yr[, .(
        equipment_type,
        age_band,
        # Weighted average maintenance_int: adopted units get the ramp,
        # non-adopted stay at baseline. unit_count used as weight.
        maint_adopted     = pmin(maintenance_int * scen_shift_yr, target_int),
        maint_not_adopted = maintenance_int / (1 + 0.07 * years_adopted), # back to baseline
        adopted_share     = adopted * 1  # 1 if adopted, 0 if not
      )]
      # Simpler: just use the adoption-state maintenance_int directly
      # (already reflects years_adopted accumulation, capped at target)
      maint_update <- state_yr[, .(
        equipment_type,
        age_band,
        maintenance_int_esg = pmin(maintenance_int * scen_shift_yr, target_int)
      )]
      
      dty <- merge(dty, maint_update, by = c("equipment_type", "age_band"), all.x = TRUE)
      dty[!is.na(maintenance_int_esg) & solar_system == "Oryn Delta",
          maintenance_int := maintenance_int_esg]
      dty[, maintenance_int_esg := NULL]
    }
    dty <- apply_ef_scenario(dty, scen_yr)
    
    n_units <- nrow(dty)
    
    # Frequency
    lambda <- predict(freq_final,
                      newdata = mutate(dty, solar_system = solar_system_proxy),
                      type    = "response") * dty$risk_scalar
    
    if (!is.null(scenario_name) && scenario_name == "solar_storm_stress") {
      sys_freq_mult_vec <- scenario$system_freq_mult[dty$solar_system]
      lambda <- lambda * sys_freq_mult_vec
    }
    
    # Simulate claim counts
    N <- matrix(rpois(sims * n_units, rep(lambda * dty$unit_count, each = sims)),
                nrow = sims)
    
    # Severity
    mu_vec <- predict(final_sev,
                      newdata = mutate(dty, solar_system = solar_system_proxy),
                      type    = "link")
    mu_mat <- matrix(rep(mu_vec, each = sims), nrow = sims)
    
    # Raw severity draws
    sev_raw <- exp(mu_mat + sigma_log * matrix(rnorm(sims * n_units), nrow = sims))
    
    # Shock depending on scenario
    if (!is.null(scenario_name) && scenario_name == "solar_storm_stress") {
      # shock_prob = 1, so shock is certain
      # Apply equipment-type-specific multipliers column-wise
      equip_mult_vec <- scenario$equip_sev_mult[dty$equipment_type]
      equip_mult_vec[is.na(equip_mult_vec)] <- scenario$shock_sev_mult  # fallback scalar
      sev_raw <- sweep(sev_raw, 2, equip_mult_vec, `*`)
      
    } else if (!is.null(scenario$shock_prob) && scenario$shock_prob > 0) {
      # Standard scalar shock path
      shock   <- rbinom(sims, 1, scenario$shock_prob)
      sev_raw <- sev_raw * (1 + scenario$shock_sev_mult * shock)
    }
    
    
    # Cumulative inflation applied to severities
    infl_vec <- econ$infl_cumul[, yr]
    sev_raw  <- sev_raw * infl_vec
    
    # Inflate deductibles and limits to year t
    ded_mat <- infl_vec %o% dty$deductible
    lim_mat <- infl_vec %o% dty$limit
    cap_mat <- pmax(lim_mat - ded_mat, 0)
    
    # Net severity after deductible and limit
    sev_net <- pmin(pmax(sev_raw - ded_mat, 0), cap_mat)
    
    # Aggregate loss: sum across units, weighted by claim counts
    sim_gross_loss       <- rowSums(sev_raw * N)
    gross_loss_mat[, yr] <- sim_gross_loss
    sim_loss       <- rowSums(sev_net * N)
    loss_mat[, yr] <- sim_loss
    
    # Discounted loss
    disc_vec          <- econ$disc_cumul[, yr]
    loss_disc_mat[, yr] <- sim_loss * disc_vec
    
    # Premium: Year 1 base, inflated thereafter
    premium_mat[, yr] <- if (yr == 1) base_premium else
      base_premium * econ$infl_cumul[, yr - 1]
    
    # ── ESG premium discount ─────────────────────────────────────────────────
    if (!is.null(adoption_state)) {
      state_yr      <- adoption_state[year == yr]
      scen_shift_yr <- scenario$maintenance_shift[yr]
      state_yr[, maint_scenario  := pmin(maintenance_int * scen_shift_yr, target_int)]
      state_yr[, eligible_final  := adopted & (maint_scenario >= target_int)]
      
      eligible_units      <- state_yr[eligible_final == TRUE, sum(unit_count)]
      total_fleet_yr      <- dty[, sum(unit_count)]
      oryn_eligible_share <- eligible_units / max(total_fleet_yr, 1)
      
      # Scalar discount applied uniformly across all sims for this year
      premium_mat[, yr] <- premium_mat[, yr] * (1 - esg_discount * oryn_eligible_share)
    }
    
    # Expense loading (front-loaded in early years, steps down after Year 3)
    expense_loading <- ifelse(yr <= 3, 0.25, 0.25 - (yr - 4) * 0.01)
    admin_rate         <- if (!is.null(scenario$admin_expense_rate)) scenario$admin_expense_rate[min(yr, length(scenario$admin_expense_rate))]
    expense_var            <- premium_mat[, yr] * (expense_loading + admin_rate)
    
    if (yr == 1) {
      fixed_expense <- 0.05 * premium_mat[, yr]
    } else {
      fixed_expense <- 0.01 * premium_mat[, yr] * infl_vec
    }
    
    expense <- expense_var + fixed_expense 
    
    # Discounted profit
    gross_profit_mat[, yr]   <- (premium_mat[, yr] - sim_gross_loss - expense) * disc_vec
    profit_mat[, yr]   <- (premium_mat[, yr] - sim_loss - expense) * disc_vec
  }
  
  # -----------------------------------------------------------------
  # AGGREGATION ACROSS YEARS
  # -----------------------------------------------------------------
  total_profit        <- rowSums(profit_mat)
  total_gross_loss_nominal  <- rowSums(gross_loss_mat)
  total_loss_nominal  <- rowSums(loss_mat)
  total_loss_discount <- rowSums(loss_disc_mat)
  total_premium       <- rowSums(premium_mat)
  loss_ratio <- total_loss_nominal / pmax(total_premium, 1e-8)
  
  q99_loss_disc <- quantile(total_loss_discount, 0.99)
  q01_profit    <- quantile(total_profit, 0.01)
  q99_loss <- quantile(total_loss_nominal, 0.99)
  q99_loss_gross <- quantile(total_gross_loss_nominal, 0.99)
  
  
  # -----------------------------------------------------------------
  # SUMMARY STATISTICS
  # -----------------------------------------------------------------
  summary_stats <- data.table(
    
    # Returns (premiums)
    EV_premium       = mean(total_premium),
    Var_premium      = var(total_premium),
    SD_premium       = sd(total_premium),
    P5_premium       = as.numeric(quantile(total_premium, 0.05)),
    P95_premium      = as.numeric(quantile(total_premium, 0.95)),
    
    # Costs — nominal
    EV_loss_nominal  = mean(total_loss_nominal),
    Var_loss_nominal = var(total_loss_nominal),
    VaR_loss = q99_loss,
    TVaR99_loss_nominal = mean(total_loss_nominal[total_loss_nominal >= q99_loss]),
    TVaR99_gross_loss_nominal = mean(total_gross_loss_nominal[total_gross_loss_nominal >= q99_loss_gross]),
    SD_loss_nominal  = sd(total_loss_nominal),
    P5_loss_nominal  = as.numeric(quantile(total_loss_nominal, 0.05)),
    P95_loss_nominal = as.numeric(quantile(total_loss_nominal, 0.95)),
    
    # Costs — discounted
    EV_loss_disc     = mean(total_loss_discount),
    Var_loss_disc    = var(total_loss_discount),
    SD_loss_disc     = sd(total_loss_discount),
    P5_loss_disc     = as.numeric(quantile(total_loss_discount, 0.05)),
    P95_loss_disc    = as.numeric(quantile(total_loss_discount, 0.95)),
    VaR99_loss_disc  = as.numeric(q99_loss_disc),
    TVaR99_loss_disc = mean(total_loss_discount[total_loss_discount >= q99_loss_disc]),
    
    # Net revenue — discounted profit
    EV_profit_disc   = mean(total_profit),
    Var_profit_disc  = var(total_profit),
    SD_profit_disc   = sd(total_profit),
    P5_profit_disc   = as.numeric(quantile(total_profit, 0.05)),
    P95_profit_disc  = as.numeric(quantile(total_profit, 0.95)),
    VaR01_profit     = as.numeric(q01_profit),
    TVaR01_profit    = mean(total_profit[total_profit <= q01_profit]),
    
    # Loss ratios
    Loss_ratio_mean  = mean(loss_ratio),
    Loss_ratio_var   = var(loss_ratio),
    Loss_ratio_sd    = sd(loss_ratio),
    Loss_ratio_p95   = as.numeric(quantile(loss_ratio, 0.95))
  )
  
  # -----------------------------------------------------------------
  # YEAR-BY-YEAR SUMMARY (short- vs long-term visibility)
  # -----------------------------------------------------------------
  yearly_summary <- data.table(
    Year       = 1:horizon,
    EV_loss_gross = colMeans(gross_loss_mat),
    EV_loss_net   = colMeans(loss_mat),
    SD_loss    = apply(loss_mat, 2, sd),
    P95_loss   = apply(loss_mat, 2, quantile, 0.95),
    EV_premium = colMeans(premium_mat),
    EV_profit  = colMeans(profit_mat),
    EV_gross_profit = colMeans(gross_profit_mat),
    SD_profit  = apply(profit_mat, 2, sd),
    P5_profit      = apply(profit_mat, 2, quantile, 0.05),
    P10_profit     = apply(profit_mat, 2, quantile, 0.10),
    P25_profit     = apply(profit_mat, 2, quantile, 0.25),
    P10_gross_profit     = apply(gross_profit_mat, 2, quantile, 0.1),
    P75_profit     = apply(profit_mat, 2, quantile, 0.75),
    P95_gross_profit     = apply(gross_profit_mat, 2, quantile, 0.95),
    P90_profit     = apply(profit_mat, 2, quantile, 0.90),
    P95_profit     = apply(profit_mat, 2, quantile, 0.95)
  )
  
  list(
    summary        = summary_stats,
    yearly         = yearly_summary,
    profit_mat     = profit_mat,
    premium_mat    = premium_mat,
    loss_mat       = loss_mat,
    gross_loss_mat = gross_loss_mat,
    gross_profit_mat = gross_profit_mat
  )
}

# -------------------------------------------------------------------------------------------------------------------
# RUN SIMULATIONS
# -------------------------------------------------------------------------------------------------------------------

# Build adoption state ONCE — shared across all scenario runs so that scenario
# differences reflect only maintenance_shift, not different random adoption draws.
# build_adoption_state() is defined in 05_esg_scenario_plot.R; source that first.
adoption_state <- build_adoption_state(
  dt            = new_business_pred_10yr,
  horizon       = 10,
  adoption_rate = 0.60,
  annual_ramp   = 0.07,
  seed          = 42
)

# Base 1-year (no ESG discount — stress/1yr runs don't pass adoption_state
# because the adoption programme hasn't had time to produce eligible units)
ef_base_1yr <- run_mc_ef(
  dt        = new_business_pred_10yr,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  seed      = 123
)
print("completed Base scenario - 1yr")

# Base 10-year with ESG discount integrated
ef_base_10yr <- run_mc_ef(
  dt             = new_business_pred_10yr,
  horizon        = 10,
  sims           = 10000,
  infl_hist      = inf_series,
  disc_hist      = rate_series,
  adoption_state = adoption_state,
  esg_discount   = 0.05,
  seed           = 123
)
print("completed Base scenario - 10yr")

# Scenario runs
ef_best <- run_mc_ef(
  dt             = new_business_pred_10yr,
  horizon        = 10,
  sims           = 10000,
  infl_hist      = inf_series,
  disc_hist      = rate_series,
  scenario       = best_scenario,
  adoption_state = adoption_state,
  esg_discount   = 0.05,
  seed           = 123
)

ef_best_1yr <- run_mc_ef(
  dt             = new_business_pred_10yr,
  horizon        = 1,
  sims           = 10000,
  infl_hist      = inf_series,
  disc_hist      = rate_series,
  scenario       = worst_scenario,
  adoption_state = adoption_state,
  esg_discount   = 0.05,
  seed           = 123
)

print("completed Best scenario - 10yr")

ef_worst <- run_mc_ef(
  dt             = new_business_pred_10yr,
  horizon        = 10,
  sims           = 10000,
  infl_hist      = inf_series,
  disc_hist      = rate_series,
  scenario       = worst_scenario,
  adoption_state = adoption_state,
  esg_discount   = 0.05,
  seed           = 123
)

ef_worst_1yr <- run_mc_ef(
  dt             = new_business_pred_10yr,
  horizon        = 1,
  sims           = 10000,
  infl_hist      = inf_series,
  disc_hist      = rate_series,
  scenario       = worst_scenario,
  adoption_state = adoption_state,
  esg_discount   = 0.05,
  seed           = 123
)

print("completed Worst scenario - 10yr")

# Stress test: 1-year horizon, no ESG discount (acute event, not long-run programme)
ef_stress <- run_mc_ef(
  dt        = new_business_pred_10yr,
  horizon   = 1,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  scenario  = solar_storm_stress,
  seed      = 123
)
print("completed Stress scenario - 1yr")

# -------------------------------------------------------------------------------------------------------------------
# RESULTS
# -------------------------------------------------------------------------------------------------------------------

ef_base_1yr$summary
ef_base_10yr$summary
ef_base_10yr$yearly

ef_best$yearly_summary
ef_worst$yearly
ef_stress$summary


combined <- bind_rows(
  ef_base_1yr$summary%>% mutate(scenario = "1-yr Base"),
  ef_base_10yr$summary %>% mutate(scenario = "10-yr Base"),
  ef_best_1yr$summary %>% mutate(scenario = "1-yr Best"),
  ef_best$summary %>% mutate(scenario = "10-yr Best"),
  ef_worst_1yr$summary %>% mutate(scenario = "1-yr Best"),
  ef_worst$summary %>% mutate(scenario = "10-yr Worst")
)

write.csv(combined, "yearly_results.csv", row.names = FALSE)
