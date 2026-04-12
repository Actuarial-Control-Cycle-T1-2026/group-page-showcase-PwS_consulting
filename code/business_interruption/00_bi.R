
# -------------------------------------------------------------------------------------------------------------------
#ACTL4001 group Assignment
#Equipment Failure model
#Sabina Xie
# -------------------------------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, readxl, MASS, fitdistrplus, actuar, MLmetrics,
               ggplot2,VGAM, vcd,gridExtra, flexsurv, vip, data.table, car, scales, pROC, caret, pscl, janitor, forecast)




# -------------------------------------------------------------------------------------------------------------------
# User inputs  
# -------------------------------------------------------------------------------------------------------------------

rm(list = ls())

#file paths
username <- Sys.info()['user']
claim_path <- paste0("/Users/", username, "/Downloads/SOA_2026_Case_Study_Materials/srcsc-2026-claims-business-interruption.xlsx")
inflation_path <- paste0("/Users/", username, "/Downloads/SOA_2026_Case_Study_Materials/srcsc-2026-interest-and-inflation.xlsx")

#Downloading claims and inventory data
df_freq <- as.data.table(read_excel(claim_path, sheet = "freq"))
df_sev <- as.data.table(read_excel(claim_path, sheet = "sev"))

#Downloading interest rate data
df_rates <- as.data.frame(read_excel(inflation_path, skip = 2) %>% clean_names())
inf_series <- df_rates$inflation
rate_series <- df_rates$x1_year_risk_free_annual_spot_rate

# -------------------------------------------------------------------------------------------------------------------
#Data cleaning 
#CHECK : EUIPMENT AGE DOESNT MAKE SENSE
# -------------------------------------------------------------------------------------------------------------------

##Cleaning frequency data -----------

df_freq_cleaned <- df_freq %>% 
  #1. Take absolute value of negative values 
  mutate(production_load = abs(production_load),
         energy_backup_score = abs(energy_backup_score),
         supply_chain_index = abs(supply_chain_index),
         avg_crew_exp = abs(avg_crew_exp),
         maintenance_freq = abs(maintenance_freq),
         safety_compliance = abs(safety_compliance),
         exposure = abs(exposure),
         claim_count = abs(claim_count)) %>%
  #2. Remove entries that are outside of the prescribed data dictionary range or NA
  filter(production_load <= 1,
         !is.na(solar_system),
         energy_backup_score <= 5,
         supply_chain_index <= 1,
         avg_crew_exp <= 30,
         maintenance_freq <= 6,
         safety_compliance <= 5,
         exposure <=1,
         claim_count <= 4) %>%
  #3. Fix naming convention for categorical variables
  mutate(solar_system = str_remove(solar_system, "_\\?\\?\\?.*"),
         station_id = str_remove(station_id, "_.*"))

(nrow(df_freq)-nrow(df_freq_cleaned))/nrow(df_freq) #1.5% of entries removed

##Cleaning severity data ----------

df_sev_cleaned <- df_sev %>% 
  #1. Take absolute value of negative values 
  mutate(production_load = abs(production_load),
         energy_backup_score = abs(energy_backup_score),
         safety_compliance = abs(safety_compliance),
         exposure = abs(exposure),
         claim_amount = abs(claim_amount)) %>%
  #2. Remove entries that are outside of the prescribed data dictionary range or NA
  filter(production_load <= 1,
         !is.na(solar_system),
         energy_backup_score <= 5,
         safety_compliance <= 5,
         # claim_amount <= 1426000,
         !is.na(claim_amount),
         exposure <=1) %>%
  #3. Fix naming convention for categorical variables
  mutate(solar_system = str_remove(solar_system, "_\\?\\?\\?.*"))

(nrow(df_sev)-nrow(df_sev_cleaned))/nrow(df_sev) #1.9% of entries removed


dispersion <- var(df_freq_cleaned$claim_count) / mean(df_freq_cleaned$claim_count)
dispersion

ggplot(df_sev_cleaned, aes(x = log(claim_amount))) +
  geom_histogram(bins = 50, fill = "blue")

fit_lognorm <- fitdist(df_sev_cleaned$claim_amount, "lnorm")
fit_gamma   <- fitdist(df_sev_cleaned$claim_amount, "gamma")

gofstat(list(fit_lognorm, fit_gamma))
# -------------------------------------------------------------------------------------------------------------------
#Preliminary Analysis 
#To do - better way to represent analysis visually for report?
# -------------------------------------------------------------------------------------------------------------------

## Frequency analysis ------------------------------

#1. Histogram of claim count
ggplot(df_freq_cleaned, aes(x = factor(claim_count))) +
  geom_bar(fill = "cornflowerblue", width = 0.7) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Claim Count Distribution", x = "Claims", y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

#2. Numerical variable predictor analysis (sense check)
df_freq_cleaned %>%
  dplyr::select(claim_count, production_load, energy_backup_score, supply_chain_index, avg_crew_exp, maintenance_freq, safety_compliance) %>%
  pivot_longer(-claim_count, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = factor(claim_count), y = value, fill = factor(claim_count))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~predictor, scales = "free_y") +
  labs(title = "Numeric Predictors vs Claim Count", x = "Claims", y = "Value")

#3. Categorical variable predictor analysis (sense check)
df_freq_cleaned %>%
  dplyr::select(claim_count,  solar_system) %>%
  pivot_longer(-claim_count, names_to = "predictor", values_to = "category") %>%
  ggplot(aes(x = category, fill = factor(claim_count))) +
  geom_bar(position = "fill", color = "white", alpha = 0.9) +
  facet_wrap(~predictor, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Claim Distribution by Categorical Predictors",
    x = "Category",y = "Proportion of Records",fill = "Claims"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major.x = element_blank())

## Severity analysis ------------------------------

#1.Distribution of claim amount
ggplot(df_sev_cleaned, aes(x = claim_amount)) +
  geom_histogram(fill = "cornflowerblue", color = "white", bins = 100) +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(
    title = "Equipment Failure Loss Distribution", 
    x = "Claim Amount", y = "Frequency"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

quantile(df_sev$claim_amount, c(0.5,0.6,0.7,0.8,0.9), na.rm = TRUE)
# Visualizing Extreme Outliers by Solar System
ggplot(df_sev_cleaned, aes(x = solar_system, y = claim_amount, fill = solar_system)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, alpha = 0.6) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "BI Severity: Extreme Outlier Analysis",
       subtitle = "The 'Red Stars' represent catastrophic losses that defy standard pricing models",
       x = "Solar System", y = "Claim Amount ($)") +
  theme_minimal()


#3. Categorical variable predictor analysis (sense check)
df_sev_cleaned %>%
  dplyr::select(claim_amount, solar_system) %>%
  pivot_longer(-claim_amount, names_to = "predictor", values_to = "category") %>%
  ggplot(aes(x = category, y = claim_amount)) +
  # Boxplots show the median and range of costs for each category
  geom_boxplot(fill = "cornflowerblue", alpha = 0.7) +
  facet_wrap(~predictor, scales = "free_x") +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "Categorical Predictors vs Claim Severity",
       subtitle = "Comparing average loss costs across different categories",
       x = "Category", y = "Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# -------------------------------
# FREQUENCY: Negative Binomial
# -------------------------------



m <- mean(df_freq_cleaned$claim_count)
v <- var(df_freq_cleaned$claim_count)
dispersion <- var(df_freq_cleaned$claim_count) / mean(df_freq_cleaned$claim_count)
#choose NB, dispersion - 1.72

# size parameter (NB dispersion)
r_nb <- m^2 / (v - m) #method of moments estimate of size
mu_nb <- m #mean claim count

# Validate: compare fitted vs observed pmf
obs_table <- table(df_freq_cleaned$claim_count) / nrow(df_freq_cleaned)
fitted_probs <- dnbinom(0:4, size = r_nb, mu = mu_nb)
names(fitted_probs) <- 0:4

data.frame(
  count   = 0:4,
  observed = as.numeric(obs_table),
  fitted   = fitted_probs
)


# -------------------------------------------------------------------------------------------------------------------
# SEVERITY: Fit marginal distribution (lognormal)
# -------------------------------------------------------------------------------------------------------------------

fit_lognorm <- fitdist(df_sev_cleaned$claim_amount, "lnorm")

# Extract lognormal parameters (BI losses are heavy-tailed)
mu_sev    <- fit_lognorm$estimate["meanlog"]
sigma_sev <- fit_lognorm$estimate["sdlog"]

# Sense check: mean of fitted lognormal vs sample mean
exp(mu_sev + sigma_sev^2 / 2) #mean of fitted lognormal - 4375710
mean(df_sev_cleaned$claim_amount) #sample mean - 4365500

#Visual check
qqcomp(fit_lognorm, main = "BI Severity: Q-Q Plot")
denscomp(fit_lognorm)

# -------------------------------------------------------------------------------------------------------------------
# Product design
# -------------------------------------------------------------------------------------------------------------------

deductible_q10 <- quantile(df_sev_cleaned$claim_amount, 0.10)
limit_q90     <- quantile(df_sev_cleaned$claim_amount, 0.9)

# -------------------------------------------------------------------------------------------------------------------
# Construct Pricing dataframe function
# -------------------------------------------------------------------------------------------------------------------

#growth (to calculate new units)
growth_model_bi <- function(system, year, h_b = 1.25, o = 1.15) {
  # Determine the total growth target for the 10-year horizon
  target_total <- case_when(
    system %in% c("Helionis Cluster", "Bayesia System") ~ h_b,
    system == "Oryn Delta" ~ o,
    TRUE ~ 1.20
  )
  weights <- c(0.20, 0.20, 0.10, 0.10, 0.10, 0.06, 0.06, 0.06, 0.06, 0.06)
  cum_growth_pct <- sum(weights[1:year])
  cum_multiplier <- 1 + (target_total - 1) * cum_growth_pct
  
  return(cum_multiplier)
}

# Base unit counts and production
system_base <- data.frame(
  solar_system      = c("Helionis Cluster", "Bayesia System", "Oryn Delta"),
  unit_count_base   = c(30, 15, 10),
  annual_production = c(375000, 250000, 125000),   # tons
  stringsAsFactors  = FALSE
) 

create_bi_dt <- function(h_b = 1.25, o = 1.15) {
  
  dt <- crossing(
    solar_system = c("Helionis Cluster", "Bayesia System", "Oryn Delta"),
    year         = 1:10
  ) %>%
    left_join(system_base %>%
                dplyr::select(solar_system, unit_count_base),
              by = "solar_system") %>%
    rowwise() %>%
    mutate(
      stations_t   = unit_count_base * growth_model_bi(solar_system, year, h_b, o),
      deductible_t = deductible_q10,
      limit_t      = limit_q90
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(
      total_stations = sum(stations_t),
      .groups = "drop"
    )
  
  return(dt)
}

# Call without passing solar_system and year as arguments
bi_dt       <- create_bi_dt()
bi_dt_best  <- create_bi_dt(h_b = 1.30, o = 1.20)
bi_dt_worst <- create_bi_dt(h_b = 1.20, o = 1.10)

# -------------------------------------------------------------------------------------------------------------------
# Run Simulations
# -------------------------------------------------------------------------------------------------------------------

source("01_sim.R")

# -------------------------------------------------------------------------------------------------------------------
# Reasonableness check
# -------------------------------------------------------------------------------------------------------------------

# Year-0 revenue at risk
total_revenue_2174 <- 61491000000
stations_newbusiness <- 30 +15+10
revenue_per_mine    <- total_revenue_2174 / stations_newbusiness
daily_rev_per_mine  <- revenue_per_mine/365
revenue_at_risk_pct <- 0.65 #assume only 60% of minue revenue genuinely stops during an outage
rev_at_risk <- daily_rev_per_mine * 0.65

#Estimate outage days from gross losses
outage_days <- mean(bi_base_1yr$gross_loss_mat) / rev_at_risk #(12.9 days)
outage_days

# -------------------------------------------------------------------------------------------------------------------
# Generate graphs
# -------------------------------------------------------------------------------------------------------------------

source("XX_plots_common.R")

# Example use for 1-year  ----------------------
plot_loss_distribution(
  product = "Business interruption",
  loss_vec  = bi_base_10yr$loss_mat[, 1], #AFTER product design
  gross_vec = bi_base_10yr$gross_loss_mat[, 1], #BEFORE product design
  year_label = "Year 1 "
)
bi_base_10yr$yearly
bi_worst$yearly
bi_best$yearly

plot_fan_gross_net(
  product        = "Business Interruption",
  gross_profit_mat  = bi_base_10yr$gross_profit_mat,
  profit_mat = bi_base_10yr$profit_mat

)

scenarios_plot(
  product = "Business Interruption",
  best_yearly = bi_best$yearly  ,
  base_yearly = bi_base_10yr$yearly,
  worst_yearly = bi_worst$yearly
)
