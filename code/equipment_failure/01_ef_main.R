
# -------------------------------------------------------------------------------------------------------------------
#ACTL4001 group Assignment
#Equipment Failure model
#Sabina Xie
# -------------------------------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, readxl, MASS, fitdistrplus, actuar, MLmetrics,
               ggplot2,vcd,gridExtra, vip, data.table, car, scales, pROC, caret, pscl, janitor, forecast)

# -------------------------------------------------------------------------------------------------------------------
# User inputs  
# -------------------------------------------------------------------------------------------------------------------

rm(list = ls())

#file paths
username <- Sys.info()['user']
claim_path <- paste0("/Users/", username, "/Downloads/SOA_2026_Case_Study_Materials/srcsc-2026-claims-equipment-failure.xlsx")
inv_path <- paste0("/Users/", username, "/Downloads/SOA_2026_Case_Study_Materials/srcsc-2026-cosmic-quarry-inventory.xlsx")
inflation_path <- paste0("/Users/", username, "/Downloads/SOA_2026_Case_Study_Materials/srcsc-2026-interest-and-inflation.xlsx")

#Downloading claims and inventory data
df_freq <- as.data.table(read_excel(claim_path, sheet = "freq"))
df_sev <- as.data.table(read_excel(claim_path, sheet = "sev"))
df_inv <- as.data.table(read_excel(inv_path))

#Downloading interest rate data
df_rates <- as.data.frame(read_excel(inflation_path, skip = 2) %>% clean_names())

# -------------------------------------------------------------------------------------------------------------------
#Data cleaning 
#CHECK : EUIPMENT AGE DOESNT MAKE SENSE
# -------------------------------------------------------------------------------------------------------------------

##Cleaning frequency data -----------

df_freq_cleaned <- df_freq %>% 
  #1. Take absolute value of negative values 
  mutate(equipment_age = abs(equipment_age),
         maintenance_int = abs(maintenance_int),
         usage_int = abs(usage_int),
         exposure = abs(exposure),
         claim_count = abs(claim_count),
         
         # LOGIC: equipment age > 10 (outside range) represent ~50% of the data; assume these were recorded 
         #        in months rather than years to maintain data volume.
         equipment_age = if_else(equipment_age > 10, equipment_age / 12, equipment_age)) %>%
  
  #2. Remove entries that are outside of the prescribed data dictionary range or NA
  filter(equipment_age >= 0 & equipment_age <= 10,
         !is.na(equipment_type),
         !is.na(solar_system),
         maintenance_int >= 100 & maintenance_int <= 5000,
         usage_int >= 0 & usage_int <= 24,
         exposure >= 0 & exposure <= 1,
         claim_count >= 0 & claim_count <= 3) %>%
  
  #3. Fix naming convention for categorical variables
  mutate(equipment_type = str_remove(equipment_type, "_\\?\\?\\?.*"),
         solar_system = str_remove(solar_system, "_\\?\\?\\?.*"))

(nrow(df_freq)-nrow(df_freq_cleaned))/nrow(df_freq) #1.7% of entries removed

##Cleaning severity data ----------

df_sev_cleaned <- df_sev %>% 
  #1. Take absolute value of negative values 
  mutate(equipment_age = abs(equipment_age),
         maintenance_int = abs(maintenance_int),
         usage_int = abs(usage_int),
         exposure = abs(exposure),
         claim_amount = abs(claim_amount),
         
         # LOGIC: equipment age > 10 (outside range) represent ~50% of the data; assume these were recorded 
         #        in months rather than years to maintain data volume.
         equipment_age = if_else(equipment_age > 10, equipment_age / 12, equipment_age)) %>%
  
  #2. Remove entries that are outside of the prescribed data dictionary range or NA
  filter(equipment_age >= 0 & equipment_age <= 10,
         maintenance_int >= 100 & maintenance_int <= 5000,
         !is.na(equipment_type),
         !is.na(solar_system),
         usage_int >= 0 & usage_int <= 24,
         exposure >= 0 & exposure <= 1,
         claim_amount >= 10500 & claim_amount <= 790500) %>% 
  
  #3. Fix naming convention for categorical variables
  mutate(equipment_type = str_remove(equipment_type, "_\\?\\?\\?.*"),
         solar_system = str_remove(solar_system, "_\\?\\?\\?.*"))

(nrow(df_sev)-nrow(df_sev_cleaned))/nrow(df_sev) #1.9% of entries removed

# -------------------------------------------------------------------------------------------------------------------
#Preliminary Analysis 
#To do - better way to represent analysis visually for report?
# -------------------------------------------------------------------------------------------------------------------
mean(df_freq_cleaned$claim_count)
var(df_freq_cleaned$claim_count)
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
  dplyr::select(claim_count, equipment_age, maintenance_int, usage_int) %>%
  pivot_longer(-claim_count, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = factor(claim_count), y = value, fill = factor(claim_count))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~predictor, scales = "free_y") +
  labs(title = "Numeric Predictors vs Claim Count", x = "Claims", y = "Value")

#3. Categorical variable predictor analysis (sense check)
df_freq_cleaned %>%
  dplyr::select(claim_count, equipment_type, solar_system) %>%
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

ggplot(df_sev_cleaned, aes(x = solar_system, y = claim_amount, fill = solar_system)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, alpha = 0.6) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "BI Severity: Extreme Outlier Analysis",
       subtitle = "The 'Red Stars' represent catastrophic losses that defy standard pricing models",
       x = "Solar System", y = "Claim Amount ($)") +
  theme_minimal()


#2. Numerical variable predictor analysis (sense check)
df_sev_cleaned %>%
  dplyr::select(claim_amount, equipment_age, maintenance_int, usage_int) %>%
  pivot_longer(-claim_amount, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = claim_amount)) +
  geom_point(alpha = 0.2, color = "cornflowerblue") + 
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  facet_wrap(~predictor, scales = "free_x") +
  scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  labs(title = "Numeric Predictors vs Claim Severity", 
       subtitle = "Scatter plots with linear trend lines",
       x = "Predictor Value", y = "Claim Amount") +
  scale_y_log10(labels = scales::label_dollar())


#3. Categorical variable predictor analysis (sense check)
df_sev_cleaned %>%
  dplyr::select(claim_amount, equipment_type, solar_system) %>%
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

# -------------------------------------------------------------------------------------------------------------------
# FREQUENCY MODEL 
# -------------------------------------------------------------------------------------------------------------------

# FREQUENCY MODEL SELECTION ---------------------------------------------

#Create training and testing set
set.seed(123)
index <- createDataPartition(df_freq_cleaned$claim_count, p = 0.8, list = FALSE)
train_data_freq <- df_freq_cleaned[index, ]
test_data_freq  <- df_freq_cleaned[-index, ]

#Candidate models - poisson, negative binomial, zero-inflated poisson
pois_mod <- glm(claim_count ~ equipment_type + equipment_age + solar_system + maintenance_int + usage_int + offset(log(exposure)), 
                family = poisson(), data = train_data_freq)

nb_mod <- glm.nb(claim_count ~ equipment_type + equipment_age + solar_system + maintenance_int + usage_int + offset(log(exposure)), 
                 data = train_data_freq)

zip_mod <- zeroinfl(claim_count ~ equipment_type + equipment_age + solar_system + maintenance_int + usage_int | 1,
                    offset = log(exposure), data = train_data_freq, dist = "poisson")

summary(pois_mod)
#AIC/BIC
data.frame(
  Model = c("Poisson", "NegBinom", "Zero-Inflated"),
  AIC = c(AIC(pois_mod), AIC(nb_mod), AIC(zip_mod)),
  BIC = c(BIC(pois_mod), BIC(nb_mod), BIC(zip_mod)),
  LogLik = c(logLik(pois_mod), logLik(nb_mod), logLik(zip_mod))
)

#Predictions
test_data_freq$pois_pred <- predict(pois_mod, newdata = test_data_freq, type = "response")
test_data_freq$nb_pred   <- predict(nb_mod, newdata = test_data_freq, type = "response")
test_data_freq$zip_pred  <- predict(zip_mod, newdata = test_data_freq, type = "response")

#AvE
AvE_calc <- function(data, pred, actual){
  AE_ratio <- sum(data[[pred]]) / sum(data[[actual]])
  return(AE_ratio)
}

AvE_calc(test_data_freq, "pois_pred","claim_count")
AvE_calc(test_data_freq, "nb_pred","claim_count")
AvE_calc(test_data_freq, "zip_pred","claim_count")

#RSME
rmse_pois <- sqrt(mean((test_data_freq$claim_count - test_data_freq$pois_pred)^2))
rmse_nb   <- sqrt(mean((test_data_freq$claim_count - test_data_freq$nb_pred)^2))
rmse_zip  <- sqrt(mean((test_data_freq$claim_count - test_data_freq$zip_pred)^2))

# FREQUENCY MODEL DEVELOPMENT ---------------------------------------------

best_base_mod <- pois_mod
# Perform stepwise selection on the best model
freq_final <- stepAIC(best_base_mod, direction = "both", trace = FALSE)

#AvE
test_data_freq$test_preds <- predict(freq_final, newdata = test_data_freq, type = "response")
total_actual <- sum(test_data_freq$claim_count)
total_pred   <- sum(test_data_freq$test_preds)
print(total_pred / total_actual)
#1.051 AvE

#deviance squared
print((freq_final$null.deviance - freq_final$deviance) / freq_final$null.deviance)

# 1. Create Deciles based on predictions
decile_data <- test_data_freq %>%
  mutate(decile = ntile(test_preds, 10)) %>% 
  group_by(decile) %>%
  summarise(
    Actual = sum(claim_count),
    Predicted = sum(test_preds),
    Exposure = sum(exposure)
  ) %>%
  # Calculate rates (optional but professional)
  mutate(
    Actual_Rate = Actual / Exposure,
    Predicted_Rate = Predicted / Exposure
  )

# 2. Plotting the Decile Backtest
ggplot(decile_data, aes(x = factor(decile))) +
  # Bars for Actuals
  geom_bar(aes(y = Actual, fill = "Actual Claims"), 
           stat = "identity", alpha = 0.7) +
  # Line and Points for Predicted
  geom_line(aes(y = Predicted, group = 1, color = "Predicted Claims"), 
            linewidth = 1) +
  geom_point(aes(y = Predicted, color = "Predicted Claims"), size = 2) +
  # Formatting
  scale_fill_manual(values = c("Actual Claims" = "grey70"), name = NULL) +
  scale_color_manual(values = c("Predicted Claims" = "#E41A1C"), name = NULL) +
  labs(
    title = "Frequency Model Decile Backtest (Poisson)",
    subtitle = "Observations ranked by predicted frequency (1 = Lowest, 10 = Highest)",
    x = "Predicted Risk Decile",
    y = "Total Claim Count",
    caption = paste0("Overall AvE Ratio: ", round(total_pred / total_actual, 3))
  ) +
  theme_minimal() + 
  theme( 
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# -------------------------------------------------------------------------------------------------------------------
# SEVERITY MODEL 
# -------------------------------------------------------------------------------------------------------------------

index_sev <- createDataPartition(df_sev_cleaned$claim_amount, p = 0.8, list = FALSE)
train_data_sev <- df_sev_cleaned[index_sev, ]
test_data_sev  <- df_sev_cleaned[-index_sev, ]

#Candidate models - gamma, log-normal, inverse gaussian, gaussian
sev_gamma <- glm(claim_amount ~ equipment_type + equipment_age + solar_system +
                   maintenance_int + usage_int,
                 family = Gamma(link="log"), data = train_data_sev)

sev_lognormal <- glm(log(claim_amount) ~ equipment_type + equipment_age + solar_system +
                       maintenance_int + usage_int,
                     family = gaussian(link="identity"), data = train_data_sev)

sev_inv_gauss <- glm(claim_amount ~ equipment_type + equipment_age + solar_system +
                       maintenance_int + usage_int,
                     family = inverse.gaussian(link="log"), data = train_data_sev)

sev_gaussian <- glm(claim_amount ~ equipment_type + equipment_age + solar_system +
                      maintenance_int + usage_int,
                    family = gaussian(link="identity"), data = train_data_sev)

#AIC/BIC
data.frame(
  Model = c("Gamma", "Log-Normal", "Inv-Gaussian", "Gaussian"),
  AIC = c(AIC(sev_gamma), AIC(sev_lognormal), AIC(sev_inv_gauss), AIC(sev_gaussian)),
  BIC = c(BIC(sev_gamma), BIC(sev_lognormal), BIC(sev_inv_gauss), BIC(sev_gaussian))
)

#Predictions
test_data_sev$gamma_pred <- predict(sev_gamma, newdata = test_data_sev, type = "response")

sigma2 <- summary(sev_lognormal)$dispersion
test_data_sev$ln_pred   <- exp(predict(sev_lognormal, newdata = test_data_sev) + sigma2/2)

test_data_sev$invgauss_pred  <- predict(sev_inv_gauss, newdata = test_data_sev, type = "response")
test_data_sev$gauss_pred  <- predict(sev_gaussian, newdata = test_data_sev, type = "response")

#AvE
AvE_calc(test_data_sev, "gamma_pred", "claim_amount")
AvE_calc(test_data_sev, "ln_pred", "claim_amount")
AvE_calc(test_data_sev, "invgauss_pred", "claim_amount")
AvE_calc(test_data_sev, "gauss_pred", "claim_amount")

#Calibration deciels
calibration_deciles_bar <- function(data, pred, actual, n_groups = 10){
  data %>%
    mutate(pred_value = .data[[pred]]) %>%
    mutate(decile = ntile(pred_value, n_groups)) %>%
    group_by(decile) %>%
    summarise(
      predicted = mean(pred_value),
      observed  = mean(.data[[actual]])
    ) %>%
    pivot_longer(cols = c(predicted, observed),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(model = pred)
}

cal_gamma <- calibration_deciles_bar(test_data_sev, "gamma_pred", "claim_amount")
cal_ln    <- calibration_deciles_bar(test_data_sev, "ln_pred", "claim_amount")
cal_ig    <- calibration_deciles_bar(test_data_sev, "invgauss_pred", "claim_amount")
cal_gauss <- calibration_deciles_bar(test_data_sev, "gauss_pred", "claim_amount")

all_cal_data <- bind_rows(
  cal_gamma %>% mutate(Model = "Gamma"),
  cal_ln    %>% mutate(Model = "Log-Normal"),
  cal_ig    %>% mutate(Model = "Inverse Gaussian"),
  cal_gauss %>% mutate(Model = "Gaussian")
)

# 2. Create the 2x2 Faceted Plot
ggplot(all_cal_data, aes(x = factor(decile), y = value, fill = type)) +
  # Use a slight dodge for the bars
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.7) +
  # Create the 2x2 grid
  facet_wrap(~ Model, scales = "free_y") + 
  # Professional Colors (Grey for Observed, Blue for Predicted)
  scale_fill_manual(values = c("observed" = "grey70", "predicted" = "#004B87"),
                    labels = c("observed" = "Actual Mean", "predicted" = "Predicted Mean")) +
  # Formatting
  scale_y_continuous(labels = scales::label_dollar(prefix = "Đ ")) +
  labs(
    title = "Severity Model Calibration: Actual vs. Predicted by Decile",
    subtitle = "Observations ranked by predicted severity; comparing four candidate distributions",
    x = "Predicted Severity Decile",
    y = "Average Claim Amount",
    fill = "Metric:"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10), # Makes facet titles bold
    panel.grid.major.x = element_blank(), # Cleans up vertical lines
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.title = element_text(face = "bold")
  )

# SEVERITY MODEL DEVELOPMENT ---------------------------------------------
best_base_mod_sev <- sev_lognormal

#stepwise selection
final_sev <- stepAIC(best_base_mod_sev, direction = "both", trace = FALSE)
summary(final_sev) #no predictors removed ^ results as above

# -------------------------------------------------------------------------------------------------------------------
# AGGREGATE LOSS PREDICTIONS - combining severity and frequency
# -------------------------------------------------------------------------------------------------------------------

#Creating loss dataset 
df_total_loss <- df_sev_cleaned %>%
  group_by(policy_id, equipment_id) %>% 
  summarise(total_actual_loss = sum(claim_amount),
            num_claims_check = n(),
            .groups = 'drop')

#Create the combined prediction table on the Frequency Test Set
test_data_combined <- test_data_freq %>%
  mutate(
    # A. Predict Frequency
    pred_freq = predict(freq_final, newdata = test_data_freq, type = "response"),
    # B. Predict Severity (Expected Cost per Claim)
    pred_sev_log = predict(final_sev, newdata = test_data_freq, type = "response"),
    pred_sev = exp(pred_sev_log + (summary(final_sev)$dispersion / 2)),
    expected_loss = pred_freq * pred_sev
  ) %>%
  left_join(df_total_loss, by = c("policy_id", "equipment_id")) %>%
  mutate(total_actual_loss = replace_na(total_actual_loss, 0))

#loss deciles comparison
test_data_combined %>%
  mutate(decile = ntile(expected_loss, 10)) %>%
  group_by(decile) %>%
  summarise(Actual = mean(total_actual_loss),
            Predicted = mean(expected_loss)) %>%
  pivot_longer(-decile) %>%
  ggplot(aes(x = decile, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Actual" = "grey50", "Predicted" = "cornflowerblue")) +
  labs(title = "Combined Model: Sorting by Decile",
       subtitle = "Proving the model separates the 'Budget Busters' from low-risk units",
       x = "Predicted Risk Decile (1=Low, 10=High)", y = "Mean Loss ($)") +
  theme_minimal()

# Creatinng the Lorenz Curve Data
lorenz_plot_data <- test_data_combined %>%
  arrange(desc(expected_loss)) %>%
  mutate(cum_actual_loss = cumsum(total_actual_loss) / sum(total_actual_loss),
         cum_records = row_number() / n())

ggplot(lorenz_plot_data, aes(x = cum_records, y = cum_actual_loss)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Lorenz Curve",
       x = "Cumulative Proportion of Fleet (Sorted by Risk)",
       y = "Cumulative Proportion of Actual Loss") +
  theme_minimal()

# -------------------------------------------------------------------------------------------------------------------
# Product Design
# -------------------------------------------------------------------------------------------------------------------

# Implement a deductible that varies based on equipment type 
# The base deductible can be the 5th percentile of historical claim amounts
# We can use the Risk index levels provided in the data to scale deductible up/down depending on solar system

equipment_deductibles <- df_sev_cleaned %>%
  group_by(equipment_type) %>%
  summarise(deductible = min(claim_amount))

#COVERAGE LIMIT
# Implement a cap on claims since historical claims has very long tail
equipment_limits <- df_sev_cleaned %>%
  group_by(equipment_type) %>%
  summarise(
    limit = quantile(claim_amount, 0.90)
  )
product_structure <- merge(equipment_deductibles, equipment_limits, by = c("equipment_type")) 




# -------------------------------------------------------------------------------------------------------------------
# Pricing new business data frame
# -------------------------------------------------------------------------------------------------------------------

source("02_projection_dt.R") 

rm(df_freq)
rm(df_sev)

# -------------------------------------------------------------------------------------------------------------------
# MONTE CARLO SIMULATION - short & long term projections
# -------------------------------------------------------------------------------------------------------------------

source("03_mc_simulation.R") 

source("XX_plots_common.R") 

# -------------------------------------------------------------------------------------------------------------------
# Graphs
# -------------------------------------------------------------------------------------------------------------------

plot_loss_distribution(
  product = "Equipment Failure",
  loss_vec  = ef_base_10yr$loss_mat[, 1], #AFTER product design
  gross_vec = ef_base_10yr$gross_loss_mat[, 1], #BEFORE product design
  year_label = "Year 1 "
)

plot_fan_gross_net(
  product        = "Equipment Failure",
  gross_profit_mat  = ef_base_10yr$gross_profit_mat,
  profit_mat = ef_base_10yr$profit_mat
)

plot_fan_gross_net_single(
  product        = "Equipment Failure",
  gross_profit_mat  = ef_base_10yr$gross_profit_mat,
  profit_mat = ef_base_10yr$profit_mat
)

scenarios_plot(
  product = "Equipment Failure",
  best_yearly = ef_best$yearly  ,
  base_yearly = ef_base_10yr$yearly,
  worst_yearly = ef_worst$yearly
               )


# Round all numeric columns to 0 dp
round_df <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ round(., 0)))
}

base_tbl  <- round_df(ef_base_10yr$yearly)
best_tbl  <- round_df(ef_best$yearly)
worst_tbl <- round_df(ef_worst$yearly)

# Create Word document
doc <- read_docx()

doc <- doc %>%
  body_add_par("Base Scenario", style = "heading 1") %>%
  body_add_flextable(flextable(base_tbl) %>% autofit()) %>%
  
  body_add_par("Best Scenario", style = "heading 1") %>%
  body_add_flextable(flextable(best_tbl) %>% autofit()) %>%
  
  body_add_par("Worst Scenario", style = "heading 1") %>%
  body_add_flextable(flextable(worst_tbl) %>% autofit())

print(doc, target = "yearly_results.docx")
