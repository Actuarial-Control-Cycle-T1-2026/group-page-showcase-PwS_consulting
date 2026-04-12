
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
[ADD CONTENT HERE]

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
| **Expected Loss** | $x | $745.8m | $3x | $212.5m |
| **Expected Total Profit** | $x | $367.2m | $x | $57.6m |
| **VaR₉₉** | $x | $862.9m | $x | $52.2m |
| **TVaR₉₉** | $x | $882.7m | $x | $59.3m |

> **Note:** The proposed designs remain cumulatively profitable over a 10-year horizon. By effectively "clipping" extreme tail exposures, the models ensure Galaxy General maintains strong solvency as interstellar exposures expand.

---
## Code Overview

---
### Data Sources and Limitations
* **Sources:** [Add sources, e.g., Cosmic Quarry Claim History]
* **Limitations & Mitigations:** [Add specific data gaps here]


### Modelling Process

**Step 1. Data Cleaning**
All four claim datasets underwent a standardised cleaning pipeline prior to modelling. Negative values in numeric fields were corrected by taking absolute values, reflecting data entry sign errors rather than genuine negative observations. Records falling outside the prescribed data dictionary ranges were removed - for example, claim counts exceeding the stated maximum, exposure values above 1, and categorical scores outside their defined scale. Categorical variables containing trailing noise strings were standardised. Across all four lines, the cleaning process removed between 1.5% and 2.0% of records, preserving the vast majority of historical experience while eliminating records that would have introduced model bias. An example of some transformations are seen below

| Cleaning Transformation Code Example  | Rationale |
| :--- | :--- |
| `df_severity[, claim_cost := abs(claim_cost)]` | Corrects sign-entry errors in variables such as severity costs and frequency counts. |
| `str_remove(solar_system, "_\\?\\?\\?.*")` | Standardizes strings to prevent join mismatches (e.g., "Zeta_???9538 --> Zeta "). |
| `df_inv[exposure > 1, exposure := 1]` | Purges illogical values based on data dictionary range. |

The process removed approximately 1.5%–2.0% of the total records, preserving the vast majority of historical experience while ensuring a high-integrity training set.

**Step 2. Model Selection and Development**

Frequency and severity models were selected independently for each line through a comparison of candidate distributions. For frequency models, specifications such as Poisson, Negative Binomial, and Zero-Inflated Poisson, were compared on metrics including AIC, BIC, and cross-validation. Where overdispersion was present, the Negative Binomial was preferred. For severity, Gamma, Log-Normal, Inverse Gaussian, and Gaussian GLMs were evaluated on AIC/BIC and aggregate calibration (Actual/Expected ratio), with decile-level discrimination plots used to assess risk differentiation. The best-performing specification was taken forward for each line, with stepwise variable selection applied to identify the most predictive set of covariates.

**Step 3. New Business Creation**

Since Cosmic Quarry's active operations span the Helionis Cluster, Bayesia System, and Oryn Delta - systems not fully represented in the historical training data - a new business pricing dataframe was constructed from the ground up for each line. Fleet inventory, equipment schedules, and operational characteristics were sourced from the Cosmic Quarry inventory file and the Online Encyclopedia Entries, and used to build a cohort-level exposure base. This dataframe was then projected forward over a 10-year horizon, with fleet/unit size evolving according to Cosmic Quarry's stated expansion targets and new units aging through the projection period.

### Step 3. New Business Creation

Since Cosmic Quarry's active operations span the Helionis Cluster, Bayesia System, and Oryn Delta, a new business pricing dataframe was constructed from the ground up. This cohort-level exposure base was projected forward over a 10-year horizon, creating a high-level 'Panel Data' structure where predictors evolve dynamically each year.

| Year | Solar System | Predictor 1 (e.g., Age) | Predictor 2 (e.g., Intensity) |
| :--- | :--- | :---: | :---: |
| 2175 | Helionis Cluster | X | X |
| 2076 | Oryn Delta | X | X |
| 2078 | Bayesia System | X | X |
| 2079 | Helionis Cluster | X | X |
| 2080 | Oryn Delta | X | X |
| ...  | ... | ... | ... |
| 2085 | Bayesia System | X | X |

**Step 4. Scenario Generation**

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

