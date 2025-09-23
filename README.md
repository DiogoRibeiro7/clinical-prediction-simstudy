# Replication of Lohmann et al. (2023): Penalization vs. Variance Decomposition in Clinical Prediction Models

This repository provides an **R implementation** of the simulation framework from:

> Lohmann, L., Groenwold, R.H.H., & van Smeden, M. (2023).
> *Comparison of likelihood penalization and variance decomposition approaches for clinical prediction models: A simulation study*.
> **Biometrical Journal**, 65(5), e700193.
> [https://doi.org/10.1002/bimj.202200193](https://doi.org/10.1002/bimj.202200193)

The goal is to replicate and extend the comparison between **likelihood penalization methods** (ridge, LASSO, elastic net, relaxed LASSO) and **variance decomposition methods** (principal component regression, partial least squares) for binary logistic prediction models, under varying sample sizes and data characteristics.

---

## 📖 Overview

The original paper showed that, in **low- to moderate-dimensional clinical settings**, **penalization approaches generally outperform variance decomposition methods**, particularly in terms of **calibration**.

This code reproduces the **full simulation grid** of the study:

* **Events per variable (EPV)**: 3, 5, 10, 15, 20, 50, 100
* **Event fraction (prevalence)**: 1/32, 1/16, 1/8, 1/4, 1/2
* **Number of predictors**: 4, 8, 16, 32, 64
* **Noise fraction**: 0, 1/4, 1/2
* **Sparse predictors**: TRUE / FALSE

This yields **1050 scenarios**, each simulated with **20 repetitions**, giving **21,000 simulated datasets** in total.

Each dataset is split into a **derivation sample** and a larger **validation sample**, allowing unbiased assessment of predictive performance.

---

## 🛠 Methods Compared

* **MLE**: unpenalized logistic regression
* **Ridge regression** (α = 0)
* **LASSO** (α = 1)
* **Elastic Net** (α ∈ {0, 0.125, 0.25, 0.5, 0.75, 1}, best CV deviance chosen)
* **Relaxed LASSO** (glmnet implementation)
* **Principal Component Regression (PCR)** with component-selection rules:

  * EVGT1: eigenvalue > 1
  * VAR90: ≥ 90% explained variance
  * AIC: minimum AIC of logistic model on PCs
  * CVDEV: minimum cross-validated deviance
* **Partial Least Squares (PLS)** with stopping rule (max 30 components, `plsRglm`)
* **Hybrid PLS + LASSO** (approximated with PCR\_cvdev scores → LASSO)

---

## 📏 Evaluation Metrics

For each method, predictions on the validation data are evaluated using:

* **Discrimination**: AUC (c-statistic)
* **Calibration slope** (winsorized to \[0.01, 10])
* **Calibration-in-the-large (CIL)**
* **Brier score**
* **rMSPE** (root mean squared prediction error vs. true probabilities)
* **MAPE** (mean absolute prediction error vs. true probabilities)

### Ranking rules (as in the paper):

* AUC and Brier scores rounded to 3 decimals before ranking
* Calibration slope rounded to 2 decimals, ranked by |slope − 1|
* Lower rank = better performance
* Ties resolved by **minimum rank**

---

## 📂 Repository Structure

```
.
├── replicate_framework.R   # Core simulation and evaluation functions
├── run_chunk.R             # Run a chunk of scenarios (parallelizable)
├── aggregate_results.R     # Combine chunk results into summaries
├── results/                # Output directory (created automatically)
└── README.md               # This file
```

---

## ▶️ Running the Simulation

### 1. Run all scenarios in chunks

You can split the 1050 scenarios into chunks (default: 50).
Each chunk is saved as an `.rds` file in the `results/` folder.

```bash
# Example: run 50 chunks, 20 iterations per scenario, 4 workers per chunk
for i in $(seq 1 50); do
  Rscript run_chunk.R --chunks 50 --chunk_id $i --iters 20 --outdir results --workers 4 &
done
wait
```

### 2. Aggregate results

After all chunks finish, combine them:

```bash
Rscript aggregate_results.R results
```

This creates:

* `summary_ranks_overall.csv` → mean ranks of all methods
* Diagnostic plots:

  * `auc_by_method.png`
  * `cal_slope_by_method.png`
  * `brier_by_method.png`

---

## 📊 Output

The final outputs include:

* **Raw results** (`.rds`) for each scenario × iteration × method
* **Per-chunk summaries** (CSV)
* **Overall method rankings** (`summary_ranks_overall.csv`)
* **Plots** showing distribution of metrics across methods

---

## 🔍 Notes on Fidelity

* **Scenario grid and iteration count** match the paper exactly.
* **Calibration slopes** are winsorized at \[0.01, 10].
* **Substitution rules**:

  * PCR with < 2 components → fallback to MLE
  * Penalized model failures → fallback to MLE
* **PLS + LASSO hybrid** uses PCR\_cvdev scores (a practical proxy).
* All random seeds are controlled for reproducibility.

---

## 📚 Citation

If you use this code or results, please cite both the **original paper** and this repository:

**Original study:**

> Lohmann, L., Groenwold, R.H.H., & van Smeden, M. (2023).
> *Comparison of likelihood penalization and variance decomposition approaches for clinical prediction models: A simulation study*.
> **Biometrical Journal**, 65(5), e700193.
> [https://doi.org/10.1002/bimj.202200193](https://doi.org/10.1002/bimj.202200193)

**This replication:**

> \[Diogo Ribeiro]. (2025). *Replication of Lohmann et al. (2023): Penalization vs. Variance Decomposition in Clinical Prediction Models*.
> GitHub repository: \[link to your repo]

---

## ⚖️ License

MIT License.
