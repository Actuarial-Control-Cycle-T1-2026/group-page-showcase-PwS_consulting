# -------------------------------------------------------------------------------------------------------------------
# NEW BUSINESS PREDICTIONS - 10-YEAR HORIZON
# Evolves the Cosmic Quarry inventory over time accounting for:
#   1. Equipment aging (age + 1 per year, capped at model limit of 10)
#   2. Fleet growth via growth_model() - new units enter at age 1
#   3. Maintenance improved for Oryn Delta via ESG incentive (adoption ramp + gap-closing)
#   4. Risk index held constant (systemic environmental risk, not time-varying)
# -------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------
# Raw dataframe
# data source : srcsc-2026-cosmic-quarry-inventory
# -------------------------------------------------------------------------------------------------------------------

new_business_raw <- tribble(
  ~solar_system, ~age_band, ~"Quantum Bore", ~"Graviton Extractor", ~"FexStram Carrier", ~"ReglAggregators", ~"Flux Rider", ~"Ion Pulverizer",
  "Helionis Cluster", "<5",    30,  24,  15,  30,  150, 9,
  "Helionis Cluster", "5-9",   45,  36,  23,  45,  225, 14,
  "Helionis Cluster", "10-14", 180, 144, 89,  180, 900, 53,
  "Helionis Cluster", "15-19", 30,  24,  15,  30,  150, 9,
  "Helionis Cluster", "20+",   15,  12,  8,   15,  75,  5,
  "Bayesia System",   "<5",    45,  36,  23,  45,  225, 14,
  "Bayesia System",   "5-9",   11,  9,   6,   11,  56,  3,
  "Bayesia System",   "10-14", 59,  48,  29,  59,  300, 18,
  "Bayesia System",   "15-19", 8,   6,   4,   8,   38,  2,
  "Bayesia System",   "20+",   0,   0,   0,   0,   0,   0,
  "Oryn Delta",       "<5",    75,  60,  37,  75,  375, 22,
  "Oryn Delta",       "5-9",   15,  12,  8,   15,  75,  5,
  "Oryn Delta",       "10-14", 10,  8,   5,   10,  50,  3,
  "Oryn Delta",       "15-19", 0,   0,   0,   0,   0,   0,
  "Oryn Delta",       "20+",   0,   0,   0,   0,   0,   0
)

# EQUIPMENT USAGE/MAINTENANCE SCHEDULES
system_schedules <- tribble(
  ~equipment_type,      ~solar_system,       ~usage_pct, ~maintenance_int,
  "Quantum Bore",       "Helionis Cluster",   0.95,        750,
  "Quantum Bore",       "Bayesia System",     0.80,        600,
  "Quantum Bore",       "Oryn Delta",         0.75,        500,
  "Graviton Extractor", "Helionis Cluster",   0.95,        750,
  "Graviton Extractor", "Bayesia System",     0.80,        600,
  "Graviton Extractor", "Oryn Delta",         0.75,        500,
  "FexStram Carrier",   "Helionis Cluster",   0.90,        375,
  "FexStram Carrier",   "Bayesia System",     0.75,        400,
  "FexStram Carrier",   "Oryn Delta",         0.70,        250,
  "ReglAggregators",    "Helionis Cluster",   0.80,       1500,
  "ReglAggregators",    "Bayesia System",     0.75,       1000,
  "ReglAggregators",    "Oryn Delta",         0.70,        300,
  "Flux Rider",         "Helionis Cluster",   0.80,       1500,
  "Flux Rider",         "Bayesia System",     0.80,       1000,
  "Flux Rider",         "Oryn Delta",         0.75,        300,
  "Ion Pulverizer",     "Helionis Cluster",   0.50,       1000,
  "Ion Pulverizer",     "Bayesia System",     0.60,        750,
  "Ion Pulverizer",     "Oryn Delta",         0.50,        500
)

# Average Risk Index (0 = No Risk, 1 = High Risk)
risk_index_raw <- tribble(
  ~equipment_type,       ~solar_system,       ~risk_index,
  "Quantum Bore",        "Helionis Cluster",   0.69,
  "Quantum Bore",        "Bayesia System",     0.77,
  "Quantum Bore",        "Oryn Delta",         0.93,
  "Graviton Extractor",  "Helionis Cluster",   0.48,
  "Graviton Extractor",  "Bayesia System",     0.57,
  "Graviton Extractor",  "Oryn Delta",         0.73,
  "FexStram Carrier",    "Helionis Cluster",   0.67,
  "FexStram Carrier",    "Bayesia System",     0.71,
  "FexStram Carrier",    "Oryn Delta",         0.78,
  "ReglAggregators",     "Helionis Cluster",   0.24,
  "ReglAggregators",     "Bayesia System",     0.26,
  "ReglAggregators",     "Oryn Delta",         0.35,
  "Flux Rider",          "Helionis Cluster",   0.24,
  "Flux Rider",          "Bayesia System",     0.21,
  "Flux Rider",          "Oryn Delta",         0.20,
  "Ion Pulverizer",      "Helionis Cluster",   0.64,
  "Ion Pulverizer",      "Bayesia System",     0.66,
  "Ion Pulverizer",      "Oryn Delta",         0.75,
)

# -------------------------------------------------------------------------------------------------------------------
# Growth model
# -------------------------------------------------------------------------------------------------------------------

growth_model <- function(system, t) {
  target_total <- case_when(
    system %in% c("Helionis Cluster", "Bayesia System") ~ 1.25,
    system == "Oryn Delta" ~ 1.15,
    TRUE ~ 1.20
  )
  weights    <- c(0.20, 0.20, 0.10, 0.10, 0.10, 0.06, 0.06, 0.06, 0.06, 0.06)
  cum_growth <- 1 + (target_total - 1) * sum(weights[1:t])
  return(cum_growth)
}

# -------------------------------------------------------------------------------------------------------------------
# Oryn Delta ESG Targets (150% of baseline for premium discount eligibility)
# -------------------------------------------------------------------------------------------------------------------

oryn_targets <- system_schedules %>%
  filter(solar_system == "Oryn Delta") %>%
  mutate(target_maintenance_int = maintenance_int * 1.50) %>%
  dplyr::select(equipment_type, 
                baseline_int = maintenance_int, 
                target_maintenance_int)

# -------------------------------------------------------------------------------------------------------------------
# Baseline inventory in long format (Year 0 snapshot)
# -------------------------------------------------------------------------------------------------------------------

new_business_base <- new_business_raw %>%
  pivot_longer(
    cols      = `Quantum Bore`:`Ion Pulverizer`,
    names_to  = "equipment_type",
    values_to = "unit_count"
  ) %>%
  mutate(equipment_age = case_when(
    age_band == "<5"    ~ 2.5,
    age_band == "5-9"   ~ 7.5,
    age_band == "10-14" ~ 10,
    age_band == "15-19" ~ 10,
    age_band == "20+"   ~ 10
  )) %>%
  left_join(system_schedules, by = c("equipment_type", "solar_system")) %>%
  mutate(
    usage_int = 24 * usage_pct,
    exposure  = 1
  ) %>%
  mutate(
    equipment_type = case_when(
      equipment_type == "FluxStream Carrier"  ~ "FexStram Carrier",
      equipment_type == "Mag-Lift Aggregator" ~ "ReglAggregators",
      equipment_type == "Fusion Transport"    ~ "Flux Rider",
      TRUE ~ equipment_type
    )
    )

# -------------------------------------------------------------------------------------------------------------------
# STEP 2: Project inventory forward year by year (Years 1-10)
# -------------------------------------------------------------------------------------------------------------------

project_inventory <- function(base, years = 10) {
  
  results <- list()
  
  for (t in seq_len(years)) {
    
    # --- A: Age existing fleet (capped at 10) ---
    aged_fleet <- base %>%
      mutate(
        equipment_age = pmin(equipment_age + t, 10),
        year          = t,
        cohort        = "existing"
      )
    
    # --- B: Compute new units added in year t vs year t-1 ---
    # growth_model returns cumulative multiplier at time t for each solar_system
    growth_t   <- mapply(growth_model, base$solar_system, MoreArgs = list(t = t))
    growth_tm1 <- if (t == 1) rep(1, nrow(base)) else
      mapply(growth_model, base$solar_system, MoreArgs = list(t = t - 1))
    
    # Incremental new units this year (may be fractional — reflects expected exposure)
    incremental_units <- base$unit_count * (growth_t - growth_tm1)
    
    new_units <- base %>%
      mutate(
        unit_count    = incremental_units,
        equipment_age = 2,        # New purchases enter at mid-point of <5 band
        year          = t,
        cohort        = "new"
      ) %>%
      filter(unit_count > 0)      # Drop zero rows (e.g., Bayesia 20+ band)
    
    results[[t]] <- bind_rows(aged_fleet, new_units)
  }
  
  bind_rows(results)
}

# -------------------------------------------------------------------------------------------------------------------
# Run 10-year projection 
# -------------------------------------------------------------------------------------------------------------------

new_business_10yr_raw <- project_inventory(new_business_base, years = 10)

# -------------------------------------------------------------------------------------------------------------------
# Attach product structure and relevant information
# -------------------------------------------------------------------------------------------------------------------

new_business_pred_10yr <- new_business_10yr_raw %>%
  
  # Fix naming convention mismatches vs training data
  mutate(
    equipment_type = case_when(
      equipment_type == "FluxStream Carrier"  ~ "FexStram Carrier",
      equipment_type == "Mag-Lift Aggregator" ~ "ReglAggregators",
      equipment_type == "Fusion Transport"    ~ "Flux Rider",
      TRUE ~ equipment_type
    ),
    # Helionis Cluster used as proxy solar system for prediction
    # (model was trained on Helionis/Epsilon/Zeta; Bayesia/Oryn not in training data)
    solar_system_proxy = "Helionis Cluster"
  ) %>%
  
  # Attach risk index (solar system + equipment type specific)
  left_join(risk_index_raw, by = c("equipment_type", "solar_system")) %>%
  
  # Attach Helionis benchmark for relative risk scalar
  left_join(
    risk_index_raw %>%
      filter(solar_system == "Helionis Cluster") %>%
      dplyr :: select(equipment_type, risk_index_hel = risk_index),
    by = "equipment_type"
  ) %>%
  mutate(risk_scalar = risk_index / risk_index_hel) %>%
  
  # Attach product structure (deductible + limit from historical distribution)
  left_join(product_structure, by = "equipment_type") %>%
  left_join(
    oryn_targets %>% dplyr::select(equipment_type, target_maintenance_int), 
    by = "equipment_type"
  )
  
  
