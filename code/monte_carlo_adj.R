run_mc_final2 <- function(dt, horizon, sims = 100000,
                          infl_hist, disc_hist,
                          phi_inf = 0.3, phi_disc = 0.3,
                          scenario = NULL,
                          seed = 123,
                          fixed_activation_pct = 0.05,
                          fixed_ongoing_pct = 0.01) {
  
  set.seed(seed)
  
  dt <- as.data.table(dt)
  dt[, deductible := fifelse(is.na(deductible), 0, deductible)]
  dt[, limit      := fifelse(is.na(limit), Inf, limit)]
  
  # Restrict to projection horizon
  dt_h     <- dt[year_clean <= horizon]
  dt_split <- split(dt_h, by = "year_clean", keep.by = FALSE)
  
  
  # Economic scenarios
  econ <- simulate_econ_ar1(
    years     = horizon,
    sims      = sims,
    infl_hist = infl_hist,
    disc_hist = disc_hist,
    phi_inf   = phi_inf,
    phi_disc  = phi_disc,
    seed      = seed
  )
  
  # -------------------------------
  # PRECOMPUTE REAL EXPECTED NET LOSS PER SHIPMENT, PER YEAR
  # -------------------------------
  EL_net_real_list <- vector("list", horizon)
  
  for (yr in 1:horizon) {
    dty <- dt_split[[as.character(yr)]]
    if (is.null(dty) || nrow(dty) == 0) next
    
    p_pos      <- predict(hurdlepois, newdata = dty, type = "response")
    lambda_pos <- predict(hurdlepois, newdata = dty, type = "count")
    freq_exp   <- p_pos * lambda_pos
    
    mu      <- predict(sev_lognormal, newdata = dty, type = "link")
    sigma2  <- summary(sev_lognormal)$dispersion
    sev_exp <- exp(mu + 0.5 * sigma2)
    
    EL_gross <- freq_exp * sev_exp
    
    ded <- dty$deductible
    lim <- dty$limit
    cap <- pmax(lim - ded, 0)
    
    EL_net_real <- pmin(pmax(EL_gross - ded, 0), cap)
    
    EL_net_real_list[[yr]] <- EL_net_real
  }
  
  # -------------------------------
  # STORAGE MATRICES
  # -------------------------------
  profit_mat          <- matrix(0, sims, horizon)
  loss_mat            <- matrix(0, sims, horizon)
  loss_disc_mat       <- matrix(0, sims, horizon)
  premium_mat         <- matrix(0, sims, horizon)
  expense_mat         <- matrix(0, sims, horizon)
  gross_loss_mat      <- matrix(0, sims, horizon)
  gross_loss_disc_mat <- matrix(0, sims, horizon)
  
  sigma <- sqrt(summary(sev_lognormal)$dispersion)
  profit_margin <- 0.10
  
  # -------------------------------
  # YEAR LOOP
  # -------------------------------
  for (yr in 1:horizon) {
    
    dty <- dt_split[[as.character(yr)]]
    if (is.null(dty) || nrow(dty) == 0) next
    
    if (!is.null(scenario)) {
      scen_year <- list(
        route_risk_shift      = scenario$route_risk_shift[yr],
        solar_radiation_shift = scenario$solar_radiation_shift[yr],
        price_mult            = scenario$price_mult[yr]
      )
      dty <- apply_scenario_drivers(
        dty,
        scen_year
      )
    }
    
    n_ship <- nrow(dty)
    
    # Frequency model
    p_pos      <- predict(hurdlepois, newdata = dty, type = "response")
    lambda_pos <- predict(hurdlepois, newdata = dty, type = "count")
    
    occ    <- matrix(rbinom(sims * n_ship, 1, p_pos), nrow = sims)
    counts <- matrix(rpois(sims * n_ship, lambda_pos), nrow = sims)
    N      <- occ * counts
    
    # Severity model (real)
    mu     <- predict(sev_lognormal, newdata = dty, type = "link")
    mu_mat <- matrix(rep(mu, each = sims), nrow = sims)
    
    sev_raw_real <- exp(mu_mat + sigma * matrix(rnorm(sims * n_ship), nrow = sims))
    
    #Correlated shock
    if (!is.null(scenario) && !is.null(scenario$shock_prob)) {
      shock <- rbinom(sims, 1, scenario$shock_prob)
      sev_raw_real <- sev_raw_real * (1 + scenario$shock_sev_mult * shock)
    }
    
    
    # Apply inflation
    infl_vec <- econ$infl_cumul[, yr]
    sev_raw  <- sev_raw_real * infl_vec
    
    # Inflate deductibles and limits
    ded_mat <- infl_vec %o% dty$deductible
    lim_mat <- infl_vec %o% dty$limit
    cap_mat <- pmax(lim_mat - ded_mat, 0)
    
    # Gross loss
    gross_loss <- rowSums(sev_raw * N)
    gross_loss_mat[, yr] <- gross_loss
    
    # Net loss
    sev_net  <- pmin(pmax(sev_raw - ded_mat, 0), cap_mat)
    sim_loss <- rowSums(sev_net * N)
    loss_mat[, yr] <- sim_loss
    
    # Discounted loss
    disc_vec <- econ$disc_cumul[, yr]
    loss_disc_mat[, yr]       <- sim_loss * disc_vec
    gross_loss_disc_mat[, yr] <- gross_loss * disc_vec
    
    # -------------------------------
    # SHIPMENT-SPECIFIC PREMIUM
    # -------------------------------
    expense_loading <- ifelse(yr <= 3, 0.25, 0.25 -(yr-3)*0.01)
    
    EL_net_real_vec <- EL_net_real_list[[yr]]
    
    EL_nominal_mat <- infl_vec %o% EL_net_real_vec
    
    premium_ship_mat <- EL_nominal_mat / (1 - expense_loading - profit_margin)
    
    premium_year_vec <- rowSums(premium_ship_mat)
    premium_mat[, yr] <- premium_year_vec
    
    # -------------------------------
    # EXPENSES (VARIABLE + FIXED)  
    # -------------------------------
    
    expense_var <- premium_year_vec * expense_loading
    
    if (yr == 1) {
      fixed_expense <- fixed_activation_pct * premium_year_vec
    } else {
      fixed_expense <- fixed_ongoing_pct * premium_year_vec * infl_vec
    }
    
    if (length(fixed_expense) != sims) {
      fixed_expense <- rep(mean(fixed_expense), sims)
    }
    
    expense <- expense_var + fixed_expense
    expense_mat[, yr] <- expense
    
    # Profit
    profit_mat[, yr] <- (premium_year_vec - sim_loss - expense) * disc_vec
  }
  
  # -------------------------------
  # AGGREGATION
  # -------------------------------
  total_profit        <- rowSums(profit_mat)
  total_loss_nominal  <- rowSums(loss_mat)
  total_loss_discount <- rowSums(loss_disc_mat)
  total_premium       <- rowSums(premium_mat)
  total_expense       <- rowSums(expense_mat)
  
  loss_ratio <- total_loss_nominal / pmax(total_premium, 1e-8)
  
  q99_loss_disc <- quantile(total_loss_discount, 0.99)
  q01_profit    <- quantile(total_profit, 0.01)
  
  summary <- data.table(
    EV_premium      = mean(total_premium),
    Var_premium     = var(total_premium),
    SD_premium      = sd(total_premium),
    P5_premium      = as.numeric(quantile(total_premium, 0.05)),
    P95_premium     = as.numeric(quantile(total_premium, 0.95)),
    
    EV_loss_nominal = mean(total_loss_nominal),
    Var_loss_nominal= var(total_loss_nominal),
    SD_loss_nominal = sd(total_loss_nominal),
    P5_loss_nominal = as.numeric(quantile(total_loss_nominal, 0.05)),
    P95_loss_nominal= as.numeric(quantile(total_loss_nominal, 0.95)),
    
    EV_loss_disc    = mean(total_loss_discount),
    Var_loss_disc   = var(total_loss_discount),
    SD_loss_disc    = sd(total_loss_discount),
    P5_loss_disc    = as.numeric(quantile(total_loss_discount, 0.05)),
    P95_loss_disc   = as.numeric(quantile(total_loss_discount, 0.95)),
    
    VaR99_loss_disc = as.numeric(q99_loss_disc),
    TVaR99_loss_disc= mean(total_loss_discount[total_loss_discount >= q99_loss_disc]),
    
    EV_profit_disc  = mean(total_profit),
    Var_profit_disc = var(total_profit),
    SD_profit_disc  = sd(total_profit),
    P5_profit_disc  = as.numeric(quantile(total_profit, 0.05)),
    P95_profit_disc = as.numeric(quantile(total_profit, 0.95)),
    
    VaR01_profit    = as.numeric(q01_profit),
    TVaR01_profit   = mean(total_profit[total_profit <= q01_profit]),
    
    Expense_loading_implied = mean(total_expense) / mean(total_premium),
    
    Loss_ratio_mean = mean(loss_ratio),
    Loss_ratio_var  = var(loss_ratio),
    Loss_ratio_sd   = sd(loss_ratio),
    Loss_ratio_p95  = as.numeric(quantile(loss_ratio, 0.95))
  )
  
  yearly_summary <- data.table(
    Year          = 1:horizon,
    EV_loss       = colMeans(loss_mat),
    SD_loss       = apply(loss_mat, 2, sd),
    P95_loss      = apply(loss_mat, 2, quantile, 0.95),
    EV_premium    = colMeans(premium_mat),
    EV_profit     = colMeans(profit_mat),
    SD_profit     = apply(profit_mat, 2, sd)
  )
  
  list(
    summary              = summary,
    yearly               = yearly_summary,
    profit_mat           = profit_mat,
    loss_mat             = loss_mat,
    loss_disc_mat        = loss_disc_mat,
    expense_mat          = expense_mat,
    premium_mat          = premium_mat,
    total_profit         = total_profit,
    total_loss_discount  = total_loss_discount,
    loss_ratio           = loss_ratio,
    gross_loss_mat       = gross_loss_mat,
    gross_loss_disc_mat  = gross_loss_disc_mat
  )
}

