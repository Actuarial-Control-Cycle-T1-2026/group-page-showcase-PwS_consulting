
# PwS Advisory - ACTL4001 Student Research Case Study
### **Actuaries in Space: The Pricing Frontier** 
**Prepared by:** Sabina Xie, Pamela Konstant, Wesley Lu  
**Date:** 30 March 2026    

---

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [Program Design](#program-design)
3. [Pricing and Capital Models](#pricing-and-capital-models)
4. [Code Overview](#code-overview)
    * [Data Sources and Limitations](#data-sources-and-limitations)
    * [Model and Code Process](#modelling-process)
    * [Simulation Engines](#simulation-engines)


---

## Executive Summary
This project develops and evaluates pricing models for **Cosmic Quarry Mining Corporation**, operating across three distinct solar systems. The analysis spans four principal hazard classes: **Equipment Failure, Cargo Loss, Workers’ Compensation, and Business Interruption**.

We assess short and long-term distributions of costs, returns, and net revenue to quantify both expected performance and tail risk exposure. The resulting framework emphasizes financial sustainability and adaptability, ensuring robustness under evolving conditions and emerging uncertainties.

**Key Deliverables:**
* Stochastic loss projections with tail risk quantification (**VaR, TVaR**).
* Scenario analysis (Base, Best, and Worst-case) and a **1-in-100 Carrington Event** stress test.
* ESG-linked pricing incentives to promote sustainable operations.

---

## Program Design

### Cargo Loss
The Cargo Loss product is a per-shipment insurance policy covering physical loss or damage to cargo during transit arising from external causes. 
* **Deductibles:** 10th percentile of observed loss by cargo type. Tiered further by container type - with higher multipler for observed high frequency containers
* **Coverage Limits:** 95th percentile of observed loss for lower value cargo. 90th percentile of observed loss for precious metals
* **Exclusions:** Cosmic Disaster events, Intergalatic War
  
### Workers' Compensation
[ADD CONTENT HERE]

### Equipment Failure
The Equipment Failure product is a per-unit indemnity policy covering sudden and unforeseen mechanical or electrical failure of extraction units across the interstellar fleet.
* **Deductibles:** Minimum observed historical claim per equipment type.
* **Coverage Limits:** 95th percentile of observed loss distribution per equipment class.
* **ESG Maintenance:** 5% premium discount for Oryn Delta policyholders who reach 150% of their 2174 baseline maintenance interval.

### Business Interruption
Uniform indemnity coverage across all solar systems triggered by a quantifiable, unplanned cessation of production output directly attributable to physical peril.
* **Deductible:** 10th percentile of observed historical claims.
* **Coverage Limit:** 90th percentile (conservative due to extreme tail).
* **Exclusions:** Deliberate shutdowns, market-driven losses, and regulatory non-compliance.

---

## Pricing and Capital Models

All models employ a **Monte Carlo stochastic framework** with 100,000 simulation paths across a 10-year projection horizon (2026–2035).

| Metric | Cargo Loss | Equipment Failure | Workers' Comp | Business Interruption |
| :--- | :---: | :---: | :---: | :---: |
| **Expected Loss** | $480.3bn | $745.8m | $x | $212.5m |
| **Expected Total Profit** | $64.8bn | $367.2m | $x | $57.6m |
| **VaR₉₉** | $532.4bn | $862.9m | $x | $52.2m |
| **TVaR₉₉** | $537.6bn | $882.7m | $x | $59.3m |

> **Note:** The proposed designs remain cumulatively profitable over a 10-year horizon. By effectively "clipping" extreme tail exposures, the models ensure Galaxy General maintains strong solvency as interstellar exposures expand.

---
## Code Overview

---
### Data Sources and Limitations
* **Sources:** [Add sources, e.g., Cosmic Quarry Claim History]
* **Limitations & Mitigations:** [Add specific data gaps here]


## Modelling Process
The following modelling process was applied consistently across each product line. Product-specific implementations of these steps are contained within the corresponding scripts:

- [Cargo Loss Model](https://github.com/Actuarial-Control-Cycle-T1-2026/group-page-showcase-PwS_consulting/tree/main/code/cargo_loss)
- [Equipment Failure Model](https://github.com/Actuarial-Control-Cycle-T1-2026/group-page-showcase-PwS_consulting/tree/main/code/equipment_failure)
- [Workers’ Compensation Model](LINK_HERE)
- [Business Interruption Model](https://github.com/Actuarial-Control-Cycle-T1-2026/group-page-showcase-PwS_consulting/tree/main/code/business_interruption)


### Step 1. Data Cleaning
All four claim datasets underwent a standardised cleaning pipeline prior to modelling. Negative values in numeric fields were corrected by taking absolute values, reflecting data entry sign errors rather than genuine negative observations. Records falling outside the prescribed data dictionary ranges were removed - for example, claim counts exceeding the stated maximum, exposure values above 1, and categorical scores outside their defined scale. Categorical variables containing trailing noise strings were standardised. Across all four lines, the cleaning process removed between 1.5% and 2.0% of records, preserving the vast majority of historical experience while eliminating records that would have introduced model bias. An example of some transformations are seen below

| Cleaning Transformation Code Example  | Rationale |
| :--- | :--- |
| `df_severity[, claim_cost := abs(claim_cost)]` | Corrects sign-entry errors in variables such as severity costs and frequency counts. |
| `str_remove(solar_system, "_\\?\\?\\?.*")` | Standardizes strings to prevent join mismatches (e.g., "Zeta_???9538 --> Zeta "). |
| `df_inv[exposure > 1, exposure := 1]` | Purges illogical values based on data dictionary range. |

The process removed approximately 1.5%–2.0% of the total records, preserving the vast majority of historical experience while ensuring a high-integrity training set.

### Step 2. Model Selection and Development

Frequency and severity models were selected independently for each line through a comparison of candidate distributions. For frequency models, specifications such as Poisson, Negative Binomial, and Zero-Inflated Poisson, were compared on metrics including AIC, BIC, and cross-validation. Where overdispersion was present, the Negative Binomial was preferred. For severity, Gamma, Log-Normal, Inverse Gaussian, and Gaussian GLMs were evaluated on AIC/BIC and aggregate calibration (Actual/Expected ratio), with decile-level discrimination plots used to assess risk differentiation. The best-performing specification was taken forward for each line, with stepwise variable selection applied to identify the most predictive set of covariates.

### Step 3. New Business Creation

Since Cosmic Quarry's active operations span the Helionis Cluster, Bayesia System, and Oryn Delta - systems not fully represented in the historical training data - a new business pricing dataframe was constructed from the ground up for each product line. Characteristics were infomred by Cosmic Quarry inventory & personnel file and the Online Encyclopedia Entries, and used to build a cohort-level exposure base. This dataframe was then projected forward over a 10-year horizon, with exposure size evolving according to Cosmic Quarry's stated expansion targets through the projection period.

Since Cosmic Quarry's active operations span the Helionis Cluster, Bayesia System, and Oryn Delta - systems not fully represented in the historical training data - a new business pricing dataframe was constructed from the ground up for each product line. 

Exposure characteristics were informed by the Cosmic Quarry inventory and personnel files, as well as Online Encyclopedia Entries, and used to build a cohort-level exposure base. This dataframe was then projected forward over a 10-year horizon, with exposure levels evolving in line with stated expansion targets over the projection period.

E.g.
| Year | Solar System | Predictor 1 (e.g., Age) | Predictor 2 (e.g., Intensity) |
| :--- | :--- | :---: | :---: |
| 2175 | Helionis Cluster | X | X |
| 2076 | Oryn Delta | X | X |
| 2078 | Bayesia System | X | X |
| 2079 | Helionis Cluster | X | X |
| 2080 | Oryn Delta | X | X |
| ...  | ... | ... | ... |
| 2085 | Bayesia System | X | X |

### Step 4. Scenario Generation

Aggregate losses, premiums, and discounted profit were projected using a Monte Carlo simulation engine across 10,000–100,000 paths per run. The framework is designed for high modularity, allowing for us to feed in different risk parameters to stress test financial outcomes over 1-year and 10-year horizons. The following code demonstrates how these parameters are bundled into a list and passed to the simulation function (run_mc) to vary the risk environment dynamically:

```r
# Scenario Parameterisation Example
best_scenario_bi <- list(
  name           = "Best Case (Smooth Operations)",
  lambda_mult    = rep(0.9, 10),
  sev_shift      = c(rep(log(0.95), 3), rep(log(0.9), 4), rep(log(0.85), 3)),
  shock_sev_mult = 0
)

# Run MC Simulation
bi_worst <- run_mc( #run_mc would be defined as the monte-carlo function 
  dt        = bi_dt_worst,
  horizon   = 10,
  sims      = 10000,
  scenario  = worst_scenario_bi,
  seed      = 123
)
```
---

## Simulation Engines

These functions supplement the [Modelling Process](#modelling-process) defined above and are adapted across each product line to support stochastic simulation of losses, premiums, expenses, and economic conditions under multiple scenario assumptions (Base, Best, Worst, and Carrington stress scenarios).

---

## Economic Assumption Simulation Function

[View Economic Assumption Function](https://github.com/Actuarial-Control-Cycle-T1-2026/group-page-showcase-PwS_consulting/blob/main/code/economic_assumptions.R)

This function simulates forward-looking paths for inflation and short-term interest rates over a 10-year horizon.  
These simulations are used in expense projections and present value calculations to assess total profit and loss.

The model generates a distribution of possible economic outcomes using Monte Carlo simulation (10,000 paths), allowing for uncertainty in future conditions rather than relying on a single deterministic forecast.

### Methodology

A bivariate autoregressive model (AR(1)) is used to jointly simulate:
- Annual inflation  
- 1-year risk-free spot rates  

Each series evolves as a mean-reverting process:

$$
x_t = \mu + \phi (x_{t-1} - \mu) + \varepsilon_t
$$

where:
- $\mu$ = long-run mean (estimated from historical data)  
- $\phi$ = persistence parameter (mean reversion strength)  
- $\varepsilon_t \sim \mathcal{N}(0, \sigma^2)$ = stochastic innovation term  

### Model Properties

The model is designed to:
- Ensure mean reversion toward historical long-run levels  
- Capture short-term volatility and persistence, ensuring shocks to inflation and interest rates persist before gradually reverting to the mean  
- Preserve the empirical relationship between inflation and interest rates via Cholesky decomposition  
- Maintain economic plausibility by flooring inflation and interest rates at 0, consistent with historical observations  

### Outputs

The function returns a list containing:
- `infl_annual`: simulated annual inflation rates  
- `disc_annual`: simulated annual interest rates  
- `infl_cumul`: cumulative inflation index  
- `disc_cumul`: cumulative discount factors  

---

## Monte Carlo Simulation Function

[View Monte Carlo Simulation Function](https://github.com/Actuarial-Control-Cycle-T1-2026/group-page-showcase-PwS_consulting/blob/main/code/monte_carlo_adj.R)

This function simulates losses, premiums, expenses, and profitability over specified horizons using a Monte Carlo framework to produce distributional outcomes for pricing, risk assessment, and capital evaluation.

### Inputs
- Frequency & Severity Models - from [Step 2. Model Selection and Development](#step-2-model-selection-and-development)
- New Business Data - from [Step 3. New Business Creation](#step-3-new-business-creation)
- Scenario Adjustor - from [Step 4. Scenario Generation](#step-4-scenario-generation)
- Economic Scenario Generator - from [Economic Assumption Simulation Function](#economic-assumption-simulation-function)
- Time Horizon (years)  

### Methodology

- **Scenario Adjustment**  
  Exposure characteristics are adjusted under defined input scenarios (Worst, Base, Best, Carrington)

- **Exposure-Level Simulation**  
  Losses are simulated per exposure using each hazard line’s frequency and severity models. Product design (limits and deductibles) is applied at the exposure level.

- **Economic Integration**  
  Inflation is applied to losses, premiums, and expenses. Discount factors are used for present value calculations.

- **Premium & Expense Modelling**  
  Premiums are derived from expected losses with expense loadings and profit margins.

- **Aggregation**  
  Results are aggregated across exposures and years to produce portfolio-level outcomes.

### Outputs

- Portfolio Summary Metrics (Expected Loss, Expected Profit, Variance, Percentiles, VaR, Loss Ratios)  
- Yearly Results (Expected loss, premium, and profit by year)  
- Full matrices of losses, premiums, expenses, and profits  

