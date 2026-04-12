### Cargo Loss Modelling ###

library(readxl)
library(ggplot2)
library(ggforce)
library(MASS)
library(data.table)
library(dplyr)
library(pscl)
library(caret)
library(tidyr)
library(scales)
library(patchwork)
library(forecast)
#====Load Data====

cargo_freq_raw <- read_xlsx("C:/Users/pamel/Downloads/srcsc-2026-claims-cargo.xlsx", sheet = "freq")
cargo_sev_raw <- read_xlsx("C:/Users/pamel/Downloads/srcsc-2026-claims-cargo.xlsx", sheet = "sev")

setDT(cargo_freq_raw)
setDT(cargo_sev_raw)

#====Freq Data Cleaning====

# Taking absolute of negative values & cleaning categorical vars
cargo_freq_raw<-cargo_freq_raw[, .(claim_count_clean = abs(claim_count),
                                   exposure_clean = abs(exposure),
                                   route_risk_clean  = abs(route_risk),
                                   cargo_value_clean = abs(cargo_value),
                                   weight_clean = abs(weight),
                                   distance_clean = abs(distance),
                                   transit_duration_clean = abs(transit_duration),
                                   debris_density_clean = abs(debris_density),
                                   solar_radiation_clean = abs(solar_radiation),
                                   vessel_age_clean = abs(vessel_age),
                                   pilot_experience_clean = abs(pilot_experience),
                                   cargo_type_clean = sub("_[^_]+$", "", cargo_type),
                                   container_type_clean = sub("_[^_]+$", "", container_type),
                                   policy_id_clean = sub("_[^_]+$", "", policy_id),
                                   shipment_id_clean = sub("_[^_]+$", "", shipment_id)
)]


# Removing uninformative data
cargo_freq_raw <- cargo_freq_raw[!is.na(claim_count_clean) & claim_count_clean <= 5,]
cargo_freq_raw <- cargo_freq_raw[!is.na(exposure_clean) & exposure_clean <= 1,]


# Applying Data Dictionary Rules
cargo_freq_raw[route_risk_clean >5, route_risk_clean:= NA]
cargo_freq_raw[, route_risk_clean:= factor(route_risk_clean)]
cargo_freq_raw[weight_clean>250000,weight_clean:=NA]
cargo_freq_raw[distance_clean>100,distance_clean:=NA]
cargo_freq_raw[pilot_experience_clean>30,pilot_experience_clean:=NA]
cargo_freq_raw[vessel_age_clean>50,vessel_age_clean:=NA]
cargo_freq_raw[solar_radiation_clean>1,solar_radiation_clean:=NA]
cargo_freq_raw[debris_density_clean>1,debris_density_clean:=NA]
cargo_freq_raw[transit_duration_clean>60,transit_duration_clean:=NA]
cargo_freq_raw[cargo_value_clean>680000000, cargo_value_clean:= NA]

#==== Freq EDA====

freq_dist <- cargo_freq_raw[,.N, by = claim_count_clean]

continuous_vars <- c(
  "cargo_value_clean",
  "weight_clean",
  "distance_clean",
  "transit_duration_clean",
  "pilot_experience_clean",
  "vessel_age_clean",
  "solar_radiation_clean",
  "debris_density_clean"
)

categorical_vars <- c("route_risk_clean", 
                      "container_type_clean",
                      "cargo_type_clean")

# Box Plots
cargo_freq_raw %>%
  dplyr::select(claim_count_clean, all_of(continuous_vars)) %>%
  pivot_longer(-claim_count_clean, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = factor(claim_count_clean), y = value, fill = factor(claim_count_clean))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~predictor, scales = "free_y") +
  labs(title = "Numeric Predictors vs Claim Count", x = "Claims", y = "Value")

# Freq Plots

freq_result <- cargo_freq_raw %>%
  pivot_longer(all_of(continuous_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(bin = ntile(value, 5)) %>%
  group_by(variable, bin) %>%
  summarise(
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    midpoint = (min(value, na.rm = TRUE) + max(value, na.rm = TRUE))/2,
    exposure = sum(exposure_clean, na.rm = TRUE),
    claims = sum(claim_count_clean),
    freq = claims / exposure,
    .groups = "drop"
  ) 

ggplot(freq_result, aes(midpoint, freq)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Claim Frequency vs Continuous Predictors",
    x = "Bin",
    y = "Claim Frequency"
  )

#Smooth Plots

cargo_freq_raw %>%
  pivot_longer(all_of(continuous_vars),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, claim_count_clean / exposure_clean,
             weight = exposure_clean)) +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Claim Frequency vs Continuous Predictors",
    x = "Predictor Value",
    y = "Claim Frequency"
  )


# Categorical Vars

freq_cat <- cargo_freq_raw %>%
  pivot_longer(all_of(categorical_vars), names_to = "variable", values_to = "category") %>%
  group_by(variable, category) %>%
  summarise(
    claims = sum(claim_count_clean, na.rm = TRUE),
    exposure = sum(exposure_clean, na.rm = TRUE),
    freq = claims / exposure,
    n = n(),  # number of rows in that category
    .groups = "drop"
  )

ggplot(freq_cat, aes(x = category, y = freq)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Claim Frequency by Categorical Variable",
       x = "Category",
       y = "Claim Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#====Frequency Model Build====

model_data_set <- cargo_freq_raw[!is.na(debris_density_clean) & !is.na(solar_radiation_clean) & !is.na(pilot_experience_clean) &!is.na(route_risk_clean) & !is.na(container_type_clean),]

set.seed(123)

index <- createDataPartition(model_data_set$claim_count_clean, p = 0.8, list = FALSE)
train_data_freq <- model_data_set[index, ]
test_data_freq  <- model_data_set[-index, ]

### Candidate models

pois_mod <- glm(claim_count_clean ~ solar_radiation_clean + pilot_experience_clean + 
                  route_risk_clean + container_type_clean + offset(log(exposure_clean)), 
                family = poisson(), data = train_data_freq)

pois_mod <- glm(claim_count_clean ~ debris_density_clean +pilot_experience_clean + 
                  route_risk_clean + container_type_clean + offset(log(exposure_clean)), 
                family = poisson(), data = train_data_freq)


nb_mod <- glm.nb(claim_count_clean ~ debris_density_clean + solar_radiation_clean + pilot_experience_clean + 
                   route_risk_clean + container_type_clean + offset(log(exposure_clean)), 
                 data = train_data_freq)

zinb_mod <- zeroinfl(claim_count_clean ~ debris_density_clean + solar_radiation_clean + pilot_experience_clean + 
                       route_risk_clean + container_type_clean + offset(log(exposure_clean)),
                     data = train_data_freq,
                     dist = "negbin"
)

zip_mod <- zeroinfl(claim_count_clean ~ debris_density_clean + solar_radiation_clean + pilot_experience_clean + 
                      route_risk_clean + container_type_clean + offset(log(exposure_clean)),
                    data = train_data_freq,
                    dist = "poisson"
)


hurdlepois <- hurdle(claim_count_clean~ solar_radiation_clean + pilot_experience_clean + 
                       route_risk_clean + container_type_clean + offset(log(exposure_clean))|solar_radiation_clean + pilot_experience_clean + 
                       route_risk_clean + container_type_clean,data=train_data_freq,dist ="poisson",zero.dist = "negbin",link= "logit")

### Model Evaluation

data.frame(
  Model = c("Poisson", "NegBinom", "Zero-Inflated_NB", "Zero-Inflated_Pois", "Hurdle_pois"),
  AIC = c(AIC(pois_mod), AIC(nb_mod), AIC(zinb_mod) ,AIC(zip_mod), AIC(hurdlepois)),
  BIC = c(BIC(pois_mod), BIC(nb_mod), BIC(zinb_mod), BIC(zip_mod), BIC(hurdlepois)),
  loglikelihood = c(logLik(pois_mod), logLik(nb_mod),logLik(zinb_mod),logLik(zip_mod), logLik(hurdlepois))
)

### Predictions
test_data_freq$pois_pred <- predict(pois_mod, newdata = test_data_freq, type = "response")
test_data_freq$nb_pred <- predict(nb_mod, newdata = test_data_freq, type = "response")
test_data_freq$zip_pred <- predict(zip_mod, newdata = test_data_freq, type = "response")
test_data_freq$znb_pred <- predict(zinb_mod, newdata = test_data_freq, type = "response")
test_data_freq$hp_pred <- predict(hurdlepois, newdata = test_data_freq, type = "response")


train_data_freq$pois_pred <- predict(pois_mod, newdata = train_data_freq, type = "response")
train_data_freq$nb_pred <- predict(nb_mod, newdata = train_data_freq, type = "response")
train_data_freq$zip_pred <- predict(zip_mod, newdata = train_data_freq, type = "response")
train_data_freq$znb_pred <- predict(zinb_mod, newdata = train_data_freq, type = "response")
train_data_freq$hp_pred <- predict(hurdlepois, newdata = train_data_freq, type = "response")

AvE_calc <- function(data, model_pred, model){
  if(model == "sev"){obs = "claim_amount_clean"}
  else {obs = "claim_count_clean"}
  expected = sum(data[[model_pred]])
  actual = sum(data[[obs]])
  AE_ratio = actual/expected
  return(AE_ratio)
}

AvE_calc(test_data_freq, "pois_pred", model = "freq")
AvE_calc(test_data_freq, "nb_pred", model = "freq")
AvE_calc(test_data_freq, "zip_pred", model = "freq")
AvE_calc(test_data_freq, "znb_pred", model = "freq")
AvE_calc(test_data_freq, "hp_pred", model = "freq")

AvE_calc(train_data_freq, "pois_pred", model = "freq")
AvE_calc(train_data_freq, "nb_pred", model = "freq")
AvE_calc(train_data_freq, "zip_pred", model = "freq")
AvE_calc(train_data_freq, "znb_pred", model = "freq")
AvE_calc(train_data_freq, "hp_pred", model = "freq")

### Plots
compute_counts <- function(model, pred, data, dist){
  
  mu <- data[[pred]]
  counts <- 0:max(data$claim_count_clean)
  # observed frequencies
  obs_counts <- table(factor(data$claim_count_clean, levels = counts))
  observed <- as.numeric(obs_counts)
  
  # expected frequencies
  if(dist == "poisson"){
    expected <- sapply(counts, function(k){
      sum(dpois(k, lambda = mu))
    })
  }
  
  if(dist == "nb"){
    theta <- model$theta
    expected <- sapply(counts, function(k){
      sum(dnbinom(k, mu = mu, size = theta))
    })
  }
  
  if(dist == "zip"){
    probs <- predict(model, newdata = data, type = "prob")
    expected <- colSums(probs[, as.character(counts)])
  }
  
  if(dist == "zinb"){
    probs <- predict(model, newdata = data, type = "prob")
    expected <- colSums(probs[, as.character(counts)])
  }
  
  if(dist == "hurdle"){
    probs <- predict(model, newdata = data, type = "prob")
    expected <- colSums(probs[, as.character(counts), drop = FALSE])
  }
  
  data.frame(
    count = counts,
    observed = observed,
    expected = expected,
    model = dist
  )
}

pois_counts <- compute_counts(pois_mod, "pois_pred", test_data_freq, dist = "poisson")
nb_counts   <- compute_counts(nb_mod,   "nb_pred",   test_data_freq, "nb")
zip_counts  <- compute_counts(zip_mod,  "zip_pred",  test_data_freq, "zip")
zinb_counts <- compute_counts(zinb_mod, "zinb_pred", test_data_freq, "zinb")
hurdle_counts <- compute_counts(hurdlepois, "hp_pred", test_data_freq, dist = "hurdle")
hurdle_counts <- compute_counts(hurdle_fit, "hb_pred", test_data_freq, dist = "hurdle")

count_data <- bind_rows(pois_counts, nb_counts, zip_counts, zinb_counts, hurdle_counts)
count_data<- bind_rows(hurdle_counts,pois_counts)
plot_data <- count_data %>%
  tidyr::pivot_longer(cols = c(observed, expected),
                      names_to = "type",
                      values_to = "frequency")

ggplot(plot_data, aes(x = factor(count), y = frequency, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~model, scales = "free_y") +
  labs(
    title = "Observed vs Predicted Claim Count Distribution",
    x = "Number of Claims",
    y = "Number of Policies",
    fill = ""
  ) +
  theme_minimal()

rmse_pois <- sqrt(mean((test_data_freq$claim_count_clean - test_data_freq$pois_pred)^2))
rmse_nb <- sqrt(mean((test_data_freq$claim_count_clean - test_data_freq$nb_pred)^2))
rmse_zip <- sqrt(mean((test_data_freq$claim_count_clean - test_data_freq$zip_pred)^2))
rmse_znb <- sqrt(mean((test_data_freq$claim_count_clean - test_data_freq$znb_pred)^2))
rmse_hp <- sqrt(mean((test_data_freq$claim_count_clean - test_data_freq$hp_pred)^2))

mae_pois <- mean(abs(test_data_freq$claim_count_clean - test_data_freq$pois_pred))
mae_nb <- mean(abs(test_data_freq$claim_count_clean - test_data_freq$nb_pred))
mae_zip <- mean(abs(test_data_freq$claim_count_clean - test_data_freq$zip_pred))
mae_znb <- mean(abs(test_data_freq$claim_count_clean - test_data_freq$znb_pred))
mae_hp <- mean(abs(test_data_freq$claim_count_clean - test_data_freq$hp_pred))




# Calibration Plots
calibration_plot <- function(data, pred, exposure, n_groups = 10){
  
  calib <- data %>%
    mutate(pred_freq = .data[[pred]]) %>%
    mutate(group = ntile(pred_freq, n_groups)) %>%
    group_by(group) %>%
    summarise(
      predicted = mean(pred_freq),
      observed = sum(claim_count_clean) / sum(.data[[exposure]]),
      exposure = sum(.data[[exposure]])
    )
  
  p <- ggplot(calib, aes(x = predicted, y = observed)) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Predicted Claim Frequency",
      y = "Observed Claim Frequency",
      title = "Model Calibration"
    ) +
    theme_minimal()
  
  return(p)
}
p1 <- calibration_plot(test_data_freq, "pois_pred", "exposure_clean")
p2 <- calibration_plot(test_data_freq, "nb_pred", "exposure_clean")
p3 <- calibration_plot(test_data_freq, "zip_pred", "exposure_clean")
p4 <- calibration_plot(test_data_freq, "znb_pred", "exposure_clean")
p5 <- calibration_plot(test_data_freq, "hp_pred", "exposure_clean")

p1 + ggtitle("Poisson")
p2 + ggtitle("Negative Binomial")
p3 + ggtitle("ZIP")
p4 + ggtitle("ZINB")
p5 + ggtitle("Hurdle")

#====Sev Data Cleaning====

# Taking absolute of negative values & cleaning categorical vars
cargo_sev_raw<-cargo_sev_raw[, .(claim_amount_clean = abs(claim_amount),
                                 exposure_clean = abs(exposure),
                                 route_risk_clean  = abs(route_risk),
                                 cargo_value_clean = abs(cargo_value),
                                 weight_clean = abs(weight),
                                 distance_clean = abs(distance),
                                 transit_duration_clean = abs(transit_duration),
                                 debris_density_clean = abs(debris_density),
                                 solar_radiation_clean = abs(solar_radiation),
                                 vessel_age_clean = abs(vessel_age),
                                 pilot_experience_clean = abs(pilot_experience),
                                 cargo_type_clean = sub("_[^_]+$", "", cargo_type),
                                 container_type_clean = sub("_[^_]+$", "", container_type),
                                 shipment_id_clean = sub("_[^_]+$", "", shipment_id),
                                 policy_id_clean = sub("_[^_]+$", "", policy_id),
                                 claim_seq_clean = abs(claim_seq))]


# Removing uninformative data
cargo_sev_raw <- cargo_sev_raw[!is.na(claim_amount_clean) & claim_amount_clean <= 678000000,]

# Applying Data Dictionary Rules
cargo_sev_raw[route_risk_clean >5, route_risk_clean:= NA]
cargo_sev_raw[, route_risk_clean:= factor(route_risk_clean)]
cargo_sev_raw[weight_clean>250000,weight_clean:=NA]
cargo_sev_raw[distance_clean>100,distance_clean:=NA]
cargo_sev_raw[pilot_experience_clean>30,pilot_experience_clean:=NA]
cargo_sev_raw[vessel_age_clean>50,vessel_age_clean:=NA]
cargo_sev_raw[solar_radiation_clean>1,solar_radiation_clean:=NA]
cargo_sev_raw[debris_density_clean>1,debris_density_clean:=NA]
cargo_sev_raw[transit_duration_clean>60,transit_duration_clean:=NA]
cargo_sev_raw[cargo_value_clean>680000000, cargo_value_clean:= NA]
cargo_sev_raw[exposure_clean>1, exposure_clean:= NA]

#Severity policy_id and shipment_id fixed

cargo_sev_clean <- copy(cargo_sev_raw)

cargo_sev_clean[, prev_ship := shift(shipment_id_clean)]
cargo_sev_clean[, next_ship := shift(shipment_id_clean, type = "lead")]
cargo_sev_clean[, prev_pol  := shift(policy_id_clean)]
cargo_sev_clean[, next_pol  := shift(policy_id_clean, type = "lead")]
cargo_sev_clean[, prev_seq  := shift(claim_seq_clean)]
cargo_sev_clean[, next_seq := shift(claim_seq_clean, type = "lead")]

cargo_sev_clean[is.na(policy_id_clean),
              policy_id_clean := fifelse(shipment_id_clean == prev_ship & claim_seq_clean!=prev_seq, prev_pol,
                                         fifelse(shipment_id_clean == next_ship & claim_seq_clean!= next_seq,
                                                 next_pol,
                                                 policy_id_clean))]

cargo_sev_clean[is.na(shipment_id_clean),
              shipment_id_clean := fifelse(policy_id_clean == prev_pol & claim_seq_clean!=prev_seq, prev_ship,
                                           fifelse(policy_id_clean == next_pol & claim_seq_clean!= next_seq,
                                                   next_ship,
                                                   shipment_id_clean))]


cargo_sev_clean[, c("prev_ship","next_ship","prev_pol","next_pol","prev_seq","next_seq") := NULL]


#==== Severity EDA ====

###BI MODEL - however when split out the precious metals, clearly see normal distribution - sufficient to have in the same model as the distribution is the same

ggplot(cargo_sev_clean, aes(claim_amount_clean)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_dollar())

ggplot(cargo_sev_clean[cargo_type_clean!="gold" &cargo_type_clean!="platinum"], aes(claim_amount_clean)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_dollar())

ggplot(cargo_sev_clean[cargo_type_clean=="gold" |cargo_type_clean=="platinum"], aes(claim_amount_clean)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_dollar())


cargo_sev_clean %>%
  dplyr::select(claim_amount_clean, all_of(continuous_vars)) %>%
  pivot_longer(-claim_amount_clean, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = claim_amount_clean)) +
  geom_point(alpha = 0.2, color = "cornflowerblue") + 
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  facet_wrap(~predictor, scales = "free_x") +
  scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  labs(title = "Numeric Predictors vs Claim Severity", 
       subtitle = "Scatter plots with linear trend lines",
       x = "Predictor Value", y = "Claim Amount") +
  scale_y_log10(labels = scales::label_dollar())
theme_minimal()

cargo_sev_clean %>%
  dplyr::select(claim_amount_clean, all_of(categorical_vars)) %>%
  pivot_longer(-claim_amount_clean, names_to = "predictor", values_to = "category") %>%
  ggplot(aes(x = category, y = claim_amount_clean)) +
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


#====Sev Models ====

sev_data_set <- cargo_sev_clean[!is.na(cargo_value_clean) & !is.na(solar_radiation_clean) & !is.na(cargo_type_clean) &!is.na(weight_clean) & !is.na(route_risk_clean),]

index_sev <- createDataPartition(sev_data_set$claim_amount_clean, p = 0.8, list = FALSE)
train_data_sev <- sev_data_set[index_sev, ]
test_data_sev  <- sev_data_set[-index_sev, ]


sev_gaussian <- glm(claim_amount_clean ~ log(cargo_value_clean) + cargo_type_clean + weight_clean + route_risk_clean + 
                      solar_radiation_clean,
                    family = gaussian(link="identity"), data = train_data_sev)


sev_gamma <- glm(claim_amount_clean ~ log(cargo_value_clean) + cargo_type_clean + weight_clean + route_risk_clean + 
                   solar_radiation_clean,
                 family = Gamma(link="log"), data = train_data_sev)


sev_inv_gauss <- glm(claim_amount_clean ~ log(cargo_value_clean) + cargo_type_clean + weight_clean + route_risk_clean + 
                       solar_radiation_clean,
                     family = inverse.gaussian(link="log"), data = train_data_sev)


sev_lognormal <- glm(log(claim_amount_clean) ~ log(cargo_value_clean) + cargo_type_clean + weight_clean + route_risk_clean + 
                       solar_radiation_clean,
                     family = gaussian(link="identity"), data = train_data_sev)

### Model Evaluation


data.frame(
  Model = c("Gaussian", "Gamma", "Inverse Gaussian", "Lognormal"),
  AIC = c(AIC(sev_gaussian), AIC(sev_gamma), AIC(sev_inv_gauss) ,AIC(sev_lognormal)),
  BIC = c(BIC(sev_gaussian), BIC(sev_gamma), BIC(sev_inv_gauss), BIC(sev_lognormal)),
  loglikelihood = c(logLik(sev_gaussian), logLik(sev_gamma),logLik(sev_inv_gauss),logLik(sev_lognormal))
)

test_data_sev$gauss_pred <- predict(sev_gaussian, newdata = test_data_sev, type = "response")
test_data_sev$gamma_pred <- predict(sev_gamma, newdata = test_data_sev, type = "response")
test_data_sev$inv_pred <- predict(sev_inv_gauss, newdata = test_data_sev, type = "response")

ln_pred <- predict(sev_lognormal, newdata = test_data_sev, type = "response")
sigma_sq <- summary(sev_lognormal)$dispersion
test_data_sev$ln_pred <- exp(ln_pred+ (sigma_sq / 2))

train_data_sev$gauss_pred <- predict(sev_gaussian, newdata = train_data_sev, type = "response")
train_data_sev$gamma_pred <- predict(sev_gamma, newdata = train_data_sev, type = "response")
train_data_sev$inv_pred <- predict(sev_inv_gauss, newdata = train_data_sev, type = "response")

ln_pred <- predict(sev_lognormal, newdata = train_data_sev, type = "response")
sigma_sq <- summary(sev_lognormal)$dispersion
train_data_sev$ln_pred <- exp(ln_pred+ (sigma_sq / 2))

AvE_calc(test_data_sev, "guass_pred", model = "sev")
AvE_calc(test_data_sev, "gamma_pred", model = "sev")
AvE_calc(test_data_sev, "inv_pred", model = "sev")
AvE_calc(test_data_sev, "ln_pred", model = "sev")

AvE_calc(train_data_sev, "guass_pred", model = "sev")
AvE_calc(train_data_sev, "gamma_pred", model = "sev")
AvE_calc(train_data_sev, "inv_pred", model = "sev")
AvE_calc(train_data_sev, "ln_pred", model = "sev")


par(mfrow = c(2,2))
qqnorm(residuals(sev_gaussian, type="deviance"))
qqline(residuals(sev_gaussian, type="deviance"), col="red")

qqnorm(residuals(sev_gamma, type="deviance"))
qqline(residuals(sev_gamma, type="deviance"), col="red")

qqnorm(residuals(sev_lognormal))
qqline(residuals(sev_lognormal), col="red")

qqnorm(residuals(sev_inv_gauss))
qqline(residuals(sev_inv_gauss), col="red")


tapply(residuals(sev_lognormal), train_data_sev$cargo_type_clean, sd)

cargo_types <- unique(train_data_sev$cargo_type_clean)

par(mfrow = c(3, 3))  # adjust depending on number of groups

for (ct in cargo_types) {
  qqnorm(residuals(sev_lognormal)[train_data_sev$cargo_type_clean == ct],
         main = paste("Q-Q Plot:", ct))
  qqline(residuals(sev_lognormal)[train_data_sev$cargo_type_clean == ct])
}

test_data_sev[cargo_type_clean=="gold"|cargo_type_clean=="platinum"] %>%
  mutate(decile = ntile(ln_pred, 10)) %>%
  group_by(decile) %>%
  summarise(Actual = mean(claim_amount_clean,na.rm = T),
            Predicted = mean(ln_pred, na.rm = T)) %>%
  pivot_longer(-decile) %>%
  ggplot(aes(x = decile, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Actual" = "grey50", "Predicted" = "cornflowerblue")) +
  labs(title = "Combined Model: Sorting by Decile",
       subtitle = "Proving the model separates the 'Budget Busters' from low-risk units",
       x = "Predicted Risk Decile (1=Low, 10=High)", y = "Mean Loss ($)") +
  theme_minimal()

test_data_sev[cargo_type_clean!="gold"&cargo_type_clean!="platinum"] %>%
  mutate(decile = ntile(ln_pred, 10)) %>%
  group_by(decile) %>%
  summarise(Actual = mean(claim_amount_clean,na.rm = T),
            Predicted = mean(ln_pred, na.rm = T)) %>%
  pivot_longer(-decile) %>%
  ggplot(aes(x = decile, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Actual" = "grey50", "Predicted" = "cornflowerblue")) +
  labs(title = "Combined Model: Sorting by Decile",
       subtitle = "Proving the model separates the 'Budget Busters' from low-risk units",
       x = "Predicted Risk Decile (1=Low, 10=High)", y = "Mean Loss ($)") +
  theme_minimal()

test_data_sev %>%
  mutate(decile = ntile(ln_pred, 10)) %>%
  group_by(decile) %>%
  summarise(Actual = mean(claim_amount_clean,na.rm = T),
            Predicted = mean(ln_pred, na.rm = T)) %>%
  pivot_longer(-decile) %>%
  ggplot(aes(x = decile, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Actual" = "grey50", "Predicted" = "cornflowerblue")) +
  labs(title = "Combined Model: Sorting by Decile",
       subtitle = "Proving the model separates the 'Budget Busters' from low-risk units",
       x = "Predicted Risk Decile (1=Low, 10=High)", y = "Mean Loss ($)") +
  theme_minimal()



## Decile Plots
calibration_data <- test_data_sev %>%
  dplyr::select(claim_amount_clean, gauss_pred, gamma_pred, inv_pred, ln_pred) %>%
  tidyr::pivot_longer(
    cols = -claim_amount_clean,
    names_to = "model",
    values_to = "prediction"
  )

# Create decile buckets for each model
calibration_summary <- calibration_data %>%
  group_by(model) %>%
  mutate(bucket = ntile(prediction, 10)) %>%
  group_by(model, bucket) %>%
  summarise(
    predicted_mean = mean(prediction),
    actual_mean = mean(claim_amount_clean),
    .groups = "drop"
  )

# Plot calibration curves
ggplot(calibration_summary, aes(x = predicted_mean, y = actual_mean)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  facet_wrap(~ model, scales = "free") +
  labs(
    title = "Severity Model Calibration Plots",
    x = "Predicted Mean Severity",
    y = "Actual Mean Severity"
  ) +
  theme_minimal()


plot_pred_vs_obs_by_decile <- function(data, pred, actual = "claim_amount_clean", n_groups = 5){
  
  calib <- data %>%
    mutate(pred_val = .data[[pred]]) %>%
    mutate(decile = ntile(pred_val, n_groups)) %>%
    group_by(decile) %>%
    summarise(
      predicted = mean(pred_val),
      observed = mean(.data[[actual]]),
      n = n()
    )
  
  ggplot(calib, aes(x = predicted, y = observed)) +
    geom_point(size = 3, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = "Average Predicted Severity",
      y = "Average Observed Severity",
      title = paste("Predicted vs Observed Severity by Decile")
    ) +
    theme_minimal()
}

# Example usage:
g1<- plot_pred_vs_obs_by_decile(test_data_sev, "gamma_pred")
g2<- plot_pred_vs_obs_by_decile(test_data_sev, "ln_pred")
g3<- plot_pred_vs_obs_by_decile(test_data_sev, "inv_pred")

(g1)/
  (g2)/
  (g3)

compare_tail_quantiles <- function(data, pred, actual = "claim_amount_clean", probs = c(0.75,0.8,0.85,0.9,0.95,0.99)){
  
  tail_df <- data.frame(
    quantile = probs,
    observed = quantile(data[[actual]], probs),
    predicted = quantile(data[[pred]], probs)
  )
  
  tail_df_long <- tail_df %>%
    tidyr::pivot_longer(cols = c("observed", "predicted"), 
                        names_to = "type", 
                        values_to = "loss")
  
  ggplot(tail_df_long, aes(x = factor(quantile), y = loss, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Quantile",
      y = "Loss Amount",
      title = "Tail Quantile Comparison"
    ) +
    theme_minimal()
}

# Example usage:
v1<- compare_tail_quantiles(test_data_sev, "gamma_pred")
v2 <- compare_tail_quantiles(test_data_sev, "ln_pred")
v3<-compare_tail_quantiles(test_data_sev, "inv_pred")

(v1)/
  (v2)/
  (v3)


library(dplyr)
library(tidyr)

get_tail_data <- function(data, pred, actual = "claim_amount_clean", probs = c(0.9, 0.95, 0.99)) {
  
  # Compute observed and predicted quantiles
  tail_df <- data.frame(
    quantile = probs,
    observed = quantile(data[[actual]], probs),
    predicted = quantile(data[[pred]], probs)
  )
  
  # Optional: long format for easy plotting or reporting
  tail_df_long <- tail_df %>%
    pivot_longer(cols = c("observed", "predicted"),
                 names_to = "type",
                 values_to = "loss")
  
  return(list(
    wide = tail_df,      # quantiles as columns
    long = tail_df_long  # good for ggplot
  ))
}

# Example usage:
tail_data <- get_tail_data(test_data_sev, "ln_pred")
tail_data <- get_tail_data(test_data_sev, "inv_pred")

# View the wide format (easy to read)
tail_data$wide

# View the long format (ready for ggplot)
tail_data$long



# -------------------------------------------------------------------------------------------------------------------
# Backesting AGGREGATE LOSS PREDICTIONS - combining severity and frequency 
# -------------------------------------------------------------------------------------------------------------------

# Aggregating Observed Losses
cargo_sev_agg <- cargo_sev_clean[
  , c(
    list(claim_amount_clean = sum(claim_amount_clean)),
    lapply(.SD, function(x) x[!is.na(x)][1])
  ),
  by = .(policy_id_clean, shipment_id_clean),
  .SDcols = setdiff(names(cargo_sev_clean),
                    c("policy_id_clean","shipment_id_clean",
                      "claim_amount_clean","claim_seq_clean"))
]

cargo_sev_agg<- cargo_sev_agg[, .(policy_id_clean, shipment_id_clean, claim_amount_clean)]



backtest_data_combined <- model_data_set[,`:=`(
  final_freq_pred = predict(hurdlepois, newdata = model_data_set, type = "response"),
  final_sev_log = predict(sev_lognormal, newdata = model_data_set, type = "response"))]

backtest_data_combined <- backtest_data_combined[, final_pred_sev :=  exp(final_sev_log + (summary(sev_lognormal)$dispersion / 2))]
#backtest_data_combined <- backtest_data_combined[, final_pred_sev :=  predict(sev_inv_gauss, newdata = model_data_set, type = "response")]

backtest_data_combined <- backtest_data_combined[,expected_loss := final_freq_pred * final_pred_sev]

backtest_data_combined[,.N, by = c("policy_id_clean", "shipment_id_clean")][N>1]

backtest_data_combined <- merge(backtest_data_combined, cargo_sev_agg, by = c("policy_id_clean", "shipment_id_clean"), all.x = TRUE)
backtest_data_combined <- backtest_data_combined[is.na(claim_amount_clean),claim_amount_clean:=0]

backtest_data_combined<- backtest_data_combined[!is.na(expected_loss)]

sum(backtest_data_combined$claim_amount_clean)/sum(backtest_data_combined$expected_loss)
sum(backtest_data_combined[cargo_type_clean!="gold" &cargo_type_clean!="platinum"]$claim_amount_clean)/sum(backtest_data_combined[cargo_type_clean!="gold" &cargo_type_clean!="platinum"]$expected_loss)
sum(backtest_data_combined[cargo_type_clean=="gold" |cargo_type_clean=="platinum"]$claim_amount_clean)/sum(backtest_data_combined[cargo_type_clean=="gold" |cargo_type_clean=="platinum"]$expected_loss)


backtest_data_combined[expected_loss<=3000000]%>%
  mutate(decile = ntile(expected_loss, 20)) %>%
  group_by(decile) %>%
  summarise(Actual = mean(claim_amount_clean,na.rm = T),
            Predicted = mean(expected_loss, na.rm = T)) %>%
  pivot_longer(-decile) %>%
  ggplot(aes(x = decile, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_manual(values = c("Actual" = "grey50", "Predicted" = "cornflowerblue")) +
  labs(title = "Combined Model: Sorting by Decile",
       x = "Predicted Risk Decile (1=Low, 20=High)", y = "Mean Loss ($)") +
  theme_minimal()


lorenz_plot_data <- backtest_data_combined %>%
  arrange(desc(expected_loss)) %>%
  mutate(cum_actual_loss = cumsum(claim_amount_clean) / sum(claim_amount_clean),
         cum_records = row_number() / n())
ggplot(lorenz_plot_data, aes(x = cum_records, y = cum_actual_loss)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Lorenz Curve: Combined Model Separation Power",
       subtitle = "Top 20% of predicted risk captures 46.4% of actual losses",
       x = "Cumulative Proportion of Fleet (Sorted by Risk)",
       y = "Cumulative Proportion of Actual Loss") +
  theme_minimal()

##Hurdle Model for Freq
##Log Normal Model for Sev

# -------------------------------------------------------------------------------------------------------------------
# New Business
# ------------------------------------------------------------------------

cargo_price_lookup <- cargo_freq_raw[
  !is.na(cargo_type_clean),
  .(
    Price = as.numeric(names(sort(table(cargo_value_clean / weight_clean), decreasing = TRUE)[1])),
    N = .N
  ),
  by = cargo_type_clean
]
setnames(cargo_price_lookup, "cargo_type_clean", "cargo")

cargo_type_weights <- head(cargo_freq_raw[,.N, by = c("cargo_type_clean","weight_clean")][order(-N)],42)

#Solar Radiation
cargo_freq_raw[!is.na(route_risk_clean), route_group :=
                 fifelse(route_risk_clean %in% c(1,2), "1-2",
                         fifelse(route_risk_clean == 3, "3", "4-5"))]

beta_params <- cargo_freq_raw[!is.na(route_risk_clean)
  , .(
    mu = mean(solar_radiation_clean, na.rm = T),
    v  = var(solar_radiation_clean, na.rm = T)
  ),
  by = route_group
]

beta_params[, alpha := mu * (mu*(1-mu)/v - 1)]
beta_params[, beta  := (1-mu) * (mu*(1-mu)/v - 1)]

par(mfrow = c(2,2))

for(r in beta_params$route_group){
  
  x <- cargo_freq_raw[route_group == r]$solar_radiation_clean
  
  p <- beta_params[route_group == r]
  
  hist(x,
       probability = TRUE,
       breaks = 20,
       col = "grey80",
       border = "white",
       main = paste("Route Risk", r))
  
  curve(dbeta(x, p$alpha, p$beta),
        add = TRUE,
        col = "red",
        lwd = 3)
}


gen_route_risk <- function(system) {
  switch(system,
         "Helionis Cluster" = sample(1:3, 1),
         "Bayesia System"     = sample(2:4, 1),
         "Oryn Delta"      = sample(4:5, 1)
  )
}

gen_pilot_exp <- function() {rnorm(1, mean = 15, sd = 4.988256)}

gen_radiation <- function(system) {
  switch(system,
         "Helionis Cluster" = rbeta(1, beta_params[route_group=="1-2"]$alpha, beta_params[route_group=="1-2"]$beta),
         "Bayesia System" = rbeta(1, beta_params[route_group=="3"]$alpha, beta_params[route_group=="3"]$beta),
         "Oryn Delta" = rbeta(1, beta_params[route_group=="4-5"]$alpha, beta_params[route_group=="4-5"]$beta))
}

pick_cargo <- function(system) {
  cargo_types <- c("gold","platinum","lithium","cobalt","supplies","titanium","rare earths")
  
  if (system %in% c("Helionis Cluster","Bayesia System")) {
    sample(cargo_types[1:6], 1)
  } else {
    sample(cargo_types, 1, prob = c(rep(0.5/6, 6), 0.5))
  }
}

pick_container <- function() {
  sample(c("DeepSpace Haulbox","DockArc Freight Case","HardSeal Transit Crate","LongHaul Vault Canister","QuantumCrate Module"), 1, prob = c(0.05, 0.10, 0.50, 0.20, 0.15))
}

cargo_weight_options <- split(
  cargo_type_weights$weight_clean,
  cargo_type_weights$cargo_type_clean
)


container_limits <- c(`DeepSpace Haulbox`=25000, `DockArc Freight Case`=50000, `HardSeal Transit Crate`=100000, `LongHaul Vault Canister`=150000, `QuantumCrate Module`=250000)
system_limits <- c(`Helionis Cluster`=375000000, `Bayesia System`=250000000, `Oryn Delta`=125000000)

gen_weight <- function(cargo, container, sys) {
  w <- sample(cargo_weight_options[[cargo]], 1)
  w <- min(w, container_limits[container])
  w <- min(w, system_limits[sys])
  return(w)
}

generate_shipments_for_system <- function(system_name, annual_tonnage) {
  
  shipments <- list()
  total_weight <- 0
  i <- 1
  
  while (total_weight < annual_tonnage) {
    
    sys <- system_name
    
    cargo <- pick_cargo(sys)          # uses your system-specific cargo rules
    cont  <- pick_container()         # same across systems
    w     <- gen_weight(cargo, cont, sys)  # respects container + system limits
    
    shipments[[i]] <- data.frame(
      solar_system     = sys,
      route_risk       = gen_route_risk(sys),
      pilot_experience = gen_pilot_exp(),
      solar_radiation  = gen_radiation(sys),
      cargo            = cargo,
      container        = cont,
      weight           = w
    )
    
    total_weight <- total_weight + w
    i <- i + 1
  }
  
  do.call(rbind, shipments)
}

shipments_Helionis <- generate_shipments_for_system(system_name = "Helionis Cluster", annual_tonnage = 375000000)
shipments_Bayesia <- generate_shipments_for_system(system_name = "Bayesia System", annual_tonnage = 250000000)
shipments_Oryn <- generate_shipments_for_system(system_name = "Oryn Delta", annual_tonnage = 125000000)

new_business_data <- rbind(shipments_Helionis, shipments_Bayesia, shipments_Oryn)
setDT(new_business_data)
new_business_data[cargo_price_lookup, Price := i.Price, on = "cargo"]
new_business_data[,cargo_value := weight*Price]
new_business_data[, exposure := 1]
new_business_data[, route_risk:= factor(route_risk)]


setnames(new_business_data, paste0(names(new_business_data), "_clean"))
setnames(new_business_data, old = c("cargo_clean","container_clean"),new = c("cargo_type_clean","container_type_clean"), skip_absent = TRUE)



new_business_data[,`:=`(
  final_freq_pred = predict(hurdlepois, newdata = new_business_data, type = "response"),
  final_sev_log = predict(sev_lognormal, newdata = new_business_data, type = "response"))]

new_business_data <- new_business_data[, final_pred_sev :=  exp(final_sev_log + (summary(sev_lognormal)$dispersion / 2))]

new_business_data <- new_business_data[,expected_loss := final_freq_pred * final_pred_sev]

sum(new_business_data$expected_loss)

unique(model_data_set$container_type_clean)
unique(new_business_data$container_type_clean)


# -------------------------------------------------------------------------------------------------------------------
# Product Design
# ---------------------------------------------------------------

cargo_limits <- cargo_sev_clean[!is.na(cargo_type_clean),.(policy_limit =quantile(claim_amount_clean,0.90)), by = cargo_type_clean]
  

cargo_quantile <- backtest_data_combined[, quantile(expected_loss, 0.1), by = "cargo_type_clean"]

container_multipler <- data.table(container_type_clean = c("HardSeal Transit Crate","QuantumCrate Module","DeepSpace Haulbox","DockArc Freight Case" ,"LongHaul Vault Canister"),
                                  multiplier = c(1.1, 1.05,1,1,1)
)

cargo_quantile[, dummy := 1]
container_multipler[, dummy := 1]

policy_deductible <- cargo_quantile[
  container_multipler,
  on = "dummy",
  allow.cartesian = TRUE
][
  , deductible := round(V1 * multiplier)
]

policy_deductible[, dummy := NULL]

# -------------------------------------------------------------------------------------------------------------------
# Monte Carlo
# --------------------------------------------------
baseline_tonnage <- c(
  "Helionis Cluster" = 375000000,
  "Bayesia System"   = 250000000,
  "Oryn Delta"       = 125000000
)

growth_model <- function(system, t) {
  # Target 10-year cumulative growth
  target_total <- case_when(
    system %in% c("Helionis Cluster", "Bayesia System") ~ 1.25,
    system == "Oryn Delta" ~ 1.15,
    TRUE ~ 1.20 #default
  )
  # Front-loaded weights
  weights <- c(0.20, 0.20, 0.10, 0.10, 0.10, 0.06, 0.06, 0.06, 0.06, 0.06) #
  cum_growth <- 1 + (target_total - 1) * sum(weights[1:t])
  return(cum_growth)
}

generate_shipments_for_year <- function(year) {
  
  systems <- names(baseline_tonnage)
  shipments_list <- list()
  
  for (sys in systems) {
    
    # Apply growth model
    growth_factor <- growth_model(sys, year)
    tonnage_t <- baseline_tonnage[sys] * growth_factor
    
    # Generate shipments for this system-year
    shipments_list[[sys]] <- generate_shipments_for_system(
      system_name   = sys,
      annual_tonnage = tonnage_t
    )
    
    # Add year column
    shipments_list[[sys]]$year <- year
  }
  
  do.call(rbind, shipments_list)
}

cargo_projection <- lapply(1:10, generate_shipments_for_year)
cargo_projection <- do.call(rbind, cargo_projection)
setDT(cargo_projection)

cargo_projection[cargo_price_lookup, Price := i.Price, on = "cargo"]
cargo_projection[,cargo_value := weight*Price]
cargo_projection[, exposure := 1]
cargo_projection[, route_risk:= factor(route_risk)]

setnames(cargo_projection, paste0(names(cargo_projection), "_clean"))
setnames(cargo_projection, old = c("cargo_clean","container_clean"),new = c("cargo_type_clean","container_type_clean"), skip_absent = TRUE)

cargo_projection[,`:=`(
  final_freq_pred = predict(hurdlepois, newdata = cargo_projection, type = "response"),
  final_sev_log = predict(sev_lognormal, newdata = cargo_projection, type = "response"))]

cargo_projection <- cargo_projection[, final_pred_sev :=  exp(final_sev_log + (summary(sev_lognormal)$dispersion / 2))]

#Add product design (deductible + limit)
cargo_projection[cargo_limits, limit:= policy_limit, on = "cargo_type_clean"]
cargo_projection[
  policy_deductible,
  deductible := i.deductible,
  on = .(cargo_type_clean, container_type_clean)]

cargo_projection_gross <- copy(cargo_projection)
cargo_projection_gross[, deductible := 0]
cargo_projection_gross[, limit := Inf]

# -------------------------------------------------------------------------------------------------------------------
# Scenario Assumptions 
# -------------------------------------------------------------------------------------------------------------------

best_scenario <- list(
  route_risk_shift = c(-0.5, -0.7, -0.9, -1.0, -1.1, -1.2, -1.3, -1.3, -1.4, -1.5),
  solar_radiation_shift = c(-0.05, -0.06, -0.07, -0.08, -0.09, -0.10, -0.11, -0.12, -0.13, -0.15),
  price_mult = c(1.00, 0.99, 0.98, 0.98, 0.97, 0.97, 0.96, 0.96, 0.95, 0.95),
  shock_prob = 0,
  shock_sev_mult = 0
)

moderate_scenario <- list(
  route_risk_shift = c(0.0, 0.0, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3),
  solar_radiation_shift = c(0.00, 0.01, 0.00, 0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.02),
  price_mult = c(1.00, 1.01, 1.02, 1.02, 1.03, 1.03, 1.04, 1.04, 1.05, 1.05),
  shock_prob = 0,
  shock_sev_mult = 0
)

worst_scenario <- list(
  route_risk_shift = c(0.5, 0.7, 0.9, 1.2, 1.5, 2.5, 1.8, 2.0, 2.2, 2.5),
  solar_radiation_shift = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.50, 0.35, 0.38, 0.40, 0.45),
  price_mult = c(1.00, 1.05, 1.10, 1.15, 1.20, 1.35, 1.25, 1.28, 1.30, 1.35),
  shock_prob = 0.1,
  shock_sev_mult = 0.5
)

carrington_stress <- list(
  route_risk_shift      = rep(0.5, 1),
  solar_radiation_shift = rep(0.5, 1),
  price_mult            = rep(1.05, 1),
  shock_sev_mult        = 0.5,
  event_share           = 0.05,
  apply_event           = TRUE
)

apply_scenario_drivers <- function(dty, scenario) {
  
  dty_adj <- copy(dty)
  
  # Route risk (1–5)
  if (!is.null(dty_adj$route_risk_clean)) {
    
    # Store original levels
    original_levels <- levels(dty_adj$route_risk_clean)
    
    # Convert to numeric
    rr_num <- as.numeric(as.character(dty_adj$route_risk_clean))
    
    # Apply shift
    rr_new <- rr_num + scenario$route_risk_shift
    
    # Clamp to 1–5
    rr_new <- pmin(5, pmax(1, round(rr_new)))
    
    # Convert back to factor with SAME levels
    dty_adj[, route_risk_clean := factor(rr_new, levels = original_levels)]
  }
  
  
  # Solar radiation (0–1)
  if (!is.null(dty_adj$solar_radiation_clean)) {
    dty_adj[, solar_radiation_clean := pmin(1, pmax(0, solar_radiation_clean + scenario$solar_radiation_shift))]
  }
  
  # Cargo value (price effect)
  if (!is.null(dty_adj$cargo_value_clean)) {
    dty_adj[, cargo_value_clean := weight_clean * Price_clean * scenario$price_mult]
  }
  
  return(dty_adj)
}

# -------------------------------------------------------------------------------------------------------------------
# Run Monte Carlo
# -------------------------------------------------------------------------------------------------------------------

# After Product Design
base_scen <- run_mc_final2(
  dt        = cargo_projection,
  horizon   = 10,
  sims      = 100000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  phi_inf   = 0.3,
  phi_disc  = 0.3,
  seed      = 67
)

#Before Product Design
base_scen_gross <- run_mc_final2(
  dt        = cargo_projection_gross,
  horizon   = 10,
  sims      = 10000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  phi_inf   = 0.3,
  phi_disc  = 0.3,
  seed      = 67
)

worse_scen <- run_mc_final2(
  dt        = cargo_projection,
  horizon   = 10,
  sims      = 100000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  phi_inf   = 0.3,
  phi_disc  = 0.3,
  scenario = worst_scenario,
  seed      = 67
)

best_scen <- run_mc_final2(
  dt        = cargo_projection,
  horizon   = 10,
  sims      = 100000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  phi_inf   = 0.3,
  phi_disc  = 0.3,
  scenario = best_scenario,
  seed      = 67
)

carrington <- run_mc_final2(
  dt        = cargo_projection,
  horizon   = 1,
  sims      = 100000,
  infl_hist = inf_series,
  disc_hist = rate_series,
  phi_inf   = 0.3,
  phi_disc  = 0.3,
  scenario = carrington_stress,
  seed      = 67
)


# -------------------------------------------------------------------------------------------------------------------
# Plots
# -------------------------------------------------------------------------------------------------------------------

plot_fan_post_expense_gross_net(
  product        = "Cargo Loss",
  loss_mat       = base_scen$loss_mat,
  gross_loss_mat = base_scen$gross_loss_mat,
  premium_mat    = base_scen$premium_mat,
  expense_mat    = base_scen$expense_mat
)

plot_loss_distribution(
  product = "Cargo Loss",
  loss_vec  = base_scen$loss_mat[, 1], #AFTER product design
  gross_vec = base_scen_gross$gross_loss_mat[, 1], #BEFORE product design
  year_label = "Year 1 "
)

plot_fan_post_expense_gross_net(
  product        = "Cargo Loss",
  loss_mat       = best_scen$loss_mat,
  gross_loss_mat = best_scen$gross_loss_mat,
  premium_mat    = best_scen$premium_mat,
  expense_mat    = best_scen$expense_mat
)


plot_fan_post_expense_gross_net(
  product        = "Cargo Loss",
  loss_mat       = worse_scen$loss_mat,
  gross_loss_mat = worse_scen$gross_loss_mat,
  premium_mat    = worse_scen$premium_mat,
  expense_mat    = worse_scen$expense_mat
)

plot_loss_distribution(
  product = "Cargo Loss",
  loss_vec  = best_scen$loss_mat[, 1], #AFTER product design
  gross_vec = best_scen$gross_loss_mat[, 1], #BEFORE product design
  year_label = "Year 1 "
)

plot_loss_distribution(
  product = "Cargo Loss",
  loss_vec  = worse_scen$loss_mat[, 1], #AFTER product design
  gross_vec = worse_scen$gross_loss_mat[, 1], #BEFORE product design
  year_label = "Year 1 "
)

scenarios_plot(
  product = "Cargo Loss",
  best_yearly = best_scen$yearly,
  base_yearly = base_scen$yearly,
  worst_yearly = worse_scen$yearly
)
