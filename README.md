[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/FxAEmrI0)

# PwS Advisory ACTL4001 Student Research Case Study
### **Actuaries in Space: The Pricing Frontier** 
**Prepared by:** Sabina Xie, Pamela Konstant, Wesley Lu  
**Date:** 30 March 2026    

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [Program Design](#program-design)
- [Pricing and Capital Models](#pricing-and-capital-models)
- [Risk Considerations](#risk-considerations)
- [Model Assumptions](#model-assumptions)
- [Data Sources and Limitations](#data-sources-and-limitations)
- [Code Development and Model Assessment](#code-development-and-model-assessment)

---

## Executive Summary

This project develops and evaluates pricing models for Cosmic Quarry Mining Corporation, operating across three distinct solar systems. The analysis spans four principal hazard classes - equipment failure, cargo loss, workers’ compensation, and business interruption - each exhibiting materially different risk characteristics across operating environments.

We assess short and long-term distributions of costs, returns, and net revenue to quantify both expected performance and tail risk exposure. These insights inform the design of targeted insurance products, calibrated to the unique operational and systemic risks within each solar system. The resulting framework emphasises both financial sustainability and adaptability, ensuring robustness under evolving conditions and emerging uncertainties.

**Key Deliverables:**
- Tailored insurance products across four hazard classes: Equipment Failure, Cargo Loss, Workers' Compensation, and Business Interruption
- Stochastic loss projections with tail risk quantification (VaR, TVaR)
- Scenario analysis under base, best-case, and worst-case conditions
- Stress testing under a 1-in-100 Carrington Event scenario
- ESG-linked pricing incentives to promote sustainable operations
- 
---

## Program Design

Each product is structured to align with operational realities while maintaining alignment with Galaxy General's risk appetite and capital constraints.

**Jump to hazard area:**
- [Cargo Loss](#cargo-loss)
- [Workers' Compensation](#workers-compensation)
- [Equipment Failure](#equipment-failure)
- [Business Interruption](#business-interruption)

---

### Cargo Loss

---

### Workers' Compensation

---

### Equipment Failure

The Equipment Failure product is a per-unit indemnity policy covering sudden and unforeseen mechanical or electrical failure of extraction units across Cosmic Quarry's interstellar fleet, with coverage triggered strictly by direct physical damage and excluding wear and tear, consequential loss, and regulatory action.

- **Deductibles:** Minimum observed historical claim per equipment type
- **Coverage Limits:** 95th percentile of observed loss distribution per equipment class
- **ESG Maintenance :** An ESG maintenance governance concession grants Oryn Delta policyholders a 5% premium discount upon reaching 150% of their 2174 baseline maintenance interval

---

### Business Interruption

The Business Interruption product is a Uniform indemnity coverage across all solar systems. The coverage is triggered if a quantifiable, unplanned cessation of production output directly attributable to physical peril.

- **Deductible:** 10th percentile of observed historical claims
- **Coverage Limit:** 90th percentile (conservative due to extreme tail)
- **Exclusions:** Deliberate shutdowns, market-driven losses, regulatory non-compliance, demand fluctuations

---

## Pricing and Capital Models

All models employ a **Monte Carlo stochastic framework** with 100,000 simulation paths across a 10-year projection horizon (2026–2035). The following metrics represent the **10-year base case projections** for the proposed insurance products.

| Metric | Cargo Loss | Equipment Failure | Workers' Comp | Business Interruption |
| :--- | :---: | :---: | :---: | :---: |
| **Expected Loss** | $x | $745.8m | $3x | $212.5m |
| **Expected Total Profit** | $x | $367.2m | $x | $57.6m |
| **VaR₉₉** | $x | $862.9m | $x | $52.2m |
| **TVaR₉₉** | $x | $882.7m | $x | $59.3m |

Across all hazard areas (Cargo Loss, Workers' Compensation, Equipment Failure, and Business Interruption), the proposed designs demonstrate structural resilience, remaining cumulatively profitable over a 10-year horizon under base case assumptions. By effectively "clipping" extreme tail exposures, the models ensure that Galaxy General maintains strong solvency and predictable financial outcomes as exposures expand across the solar systems.

---

## Risk Considerations


---

## Model Assumptions

### Economic Assumptions

---

### Pricing & Profitability Assumptions

---

### New Business Growth Assumptions

---

## Data Sources and Limitations

### Data Sources

### Known Data Limitations & Mitigations

---

## Code Development and Model Assessment

**Step 1. Data Cleaning**
All four claim datasets underwent a standardised cleaning pipeline prior to modelling. Negative values in numeric fields were corrected by taking absolute values, reflecting data entry sign errors rather than genuine negative observations. Records falling outside the prescribed data dictionary ranges were removed - for example, claim counts exceeding the stated maximum, exposure values above 1, and categorical scores outside their defined scale. Categorical variables containing trailing noise strings were standardised. Across all four lines, the cleaning process removed between 1.5% and 2.0% of records, preserving the vast majority of historical experience while eliminating records that would have introduced model bias.

**Step 2. Model Selection and Development**

Frequency and severity models were selected independently for each line through a comparison of candidate distributions. For frequency models, specifications such as Poisson, Negative Binomial, and Zero-Inflated Poisson, were compared on metrics including AIC, BIC, and cross-validation. Where overdispersion was present, the Negative Binomial was preferred. For severity, Gamma, Log-Normal, Inverse Gaussian, and Gaussian GLMs were evaluated on AIC/BIC and aggregate calibration (Actual/Expected ratio), with decile-level discrimination plots used to assess risk differentiation. The best-performing specification was taken forward for each line, with stepwise variable selection applied to identify the most predictive set of covariates.

**Step 3. New Business Creation**

Since Cosmic Quarry's active operations span the Helionis Cluster, Bayesia System, and Oryn Delta - systems not fully represented in the historical training data - a new business pricing dataframe was constructed from the ground up for each line. Fleet inventory, equipment schedules, and operational characteristics were sourced from the Cosmic Quarry inventory file and the Online Encyclopedia Entries, and used to build a cohort-level exposure base. This dataframe was then projected forward over a 10-year horizon, with fleet/unit size evolving according to Cosmic Quarry's stated expansion targets and new units aging through the projection period.

**Step 4. Scenario Generation**

Using a Monte Carlo simulation across 10,000–100,000 paths per run, aggregate losses, premiums, and discounted profit were projected over both a 1-year and 10-year horizon. Three scenarios were constructed for each line: a base case where model predictions are realised exactly; a best case assuming an improving risk environment; and a worst case assuming deteriorating conditions. A separate Carrington-scale stress test was constructed for each hazard line to model a 1-in-100 year correlated solar event, with system-specific frequency multipliers and severity loadings grounded in each solar system's described stellar characteristics.

---


---

