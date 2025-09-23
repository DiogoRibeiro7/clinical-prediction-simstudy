# Replication of Lohmann et al. (2023): Penalization vs. Variance Decomposition in Clinical Prediction Models

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)

> **Complete R implementation** of the simulation framework from Lohmann, Groenwold & van Smeden (2023) comparing likelihood penalization and variance decomposition approaches for clinical prediction models.

## 📄 Original Study

**Citation:**
> Lohmann, L., Groenwold, R.H.H., & van Smeden, M. (2023).  
> *Comparison of likelihood penalization and variance decomposition approaches for clinical prediction models: A simulation study*.  
> **Biometrical Journal**, 65(5), e700193.  
> [https://doi.org/10.1002/bimj.202200108](https://doi.org/10.1002/bimj.202200108)

**Key Finding:** In low- to moderate-dimensional clinical settings, **penalization approaches generally outperform variance decomposition methods**, particularly in terms of calibration.

---

## 🎯 Objectives

This repository provides a **complete replication** of the original study to:

1. **Validate** the original findings using independent R implementation
2. **Extend** the analysis with additional diagnostic plots and summaries
3. **Enable** reproducible research with parallelized, robust code
4. **Compare** 11 different modeling approaches across 1,050 scenarios

---

## 🔬 Simulation Design

### Experimental Grid (1,050 Total Scenarios)

| Parameter | Values | Count |
|-----------|--------|-------|
| **Events per Variable (EPV)** | 3, 5, 10, 15, 20, 50, 100 | 7 |
| **Event Fraction** | 1/32, 1/16, 1/8, 1/4, 1/2 | 5 |
| **Predictors (p)** | 4, 8, 16, 32, 64 | 5 |
| **Noise Fraction** | 0, 1/4, 1/2 | 3 |
| **Sparse Predictors** | FALSE, TRUE | 2 |

**Total:** 7 × 5 × 5 × 3 × 2 = **1,050 scenarios**  
**Iterations:** 20 per scenario = **21,000 datasets**  
**Evaluations:** 11 methods × 21,000 = **231,000 model fits**

### Data Generation Process

1. **Correlation Structure**: Random correlation matrix with Beta(1,3) off-diagonal elements
2. **Predictor Generation**: Multivariate normal with structured correlation
3. **Sparse Transformation**: 25% of predictors converted to binary (if `sparse = TRUE`)
4. **Coefficients**: Standard normal with controlled noise fraction
5. **Sample Sizes**: 
   - Training: `max(200, ⌈(EPV × p) / event_fraction⌉)`
   - Validation: `max(200, ⌈20 × p / event_fraction⌉)` (large for unbiased assessment)

---

## 🛠 Methods Compared

### Penalization Approaches
| Method | Description | Implementation |
|--------|-------------|----------------|
| **MLE** | Unpenalized logistic regression | `glm()` |
| **Ridge** | L2 penalty (α = 0) | `glmnet` with CV |
| **LASSO** | L1 penalty (α = 1) | `glmnet` with CV |
| **Elastic Net** | Combined L1+L2 penalty | Best α ∈ {0, 0.125, 0.25, 0.5, 0.75, 1} |
| **Relaxed LASSO** | Two-step LASSO procedure | `glmnet` with `relax = TRUE` |

### Variance Decomposition Approaches
| Method | Description | Component Selection |
|--------|-------------|-------------------|
| **PCR-EVGT1** | Principal Components | Eigenvalue > 1 |
| **PCR-VAR90** | Principal Components | ≥90% variance explained |
| **PCR-AIC** | Principal Components | Minimum AIC |
| **PCR-CVDEV** | Principal Components | Min. cross-validated deviance |
| **PLS** | Partial Least Squares | Sparse stopping rule (max 30) |
| **PLS-LASSO** | Hybrid approach | PLS scores → LASSO (approximated) |

---

## 📊 Evaluation Metrics

All methods evaluated on **independent validation data**:

| Metric | Description | Ranking Rule |
|--------|-------------|--------------|
| **AUC** | Discrimination (c-statistic) | Higher better, 3 decimals |
| **Calibration Slope** | Logit(pᵢ) coefficient | Closer to 1 better, 2 decimals |
| **Calibration-in-Large** | Intercept in offset model | Closer to 0 better |
| **Brier Score** | Mean squared prediction error | Lower better, 3 decimals |
| **rMSPE** | Root MSE vs. true probabilities | Lower better |
| **MAPE** | Mean absolute error vs. true probabilities | Lower better |

**Ranking System:** Follows original paper's rounding rules with minimum-rank tie breaking.

---

## 🚀 Quick Start

### Prerequisites

```r
# Required R packages
install.packages(c("MASS", "glmnet", "pROC", "plsRglm", "Matrix", 
                   "dplyr", "purrr", "tibble", "tidyr", "readr", 
                   "ggplot2", "glue", "optparse", "future.apply"))
```

### Option 1: Run Complete Simulation

```bash
# Clone repository
git clone [your-repo-url]
cd replication-lohmann-2023

# Run all 1,050 scenarios in 50 parallel chunks
for i in $(seq 1 50); do
  Rscript run_chunk.R --chunks 50 --chunk_id $i --iters 20 --workers 4 &
done
wait

# Aggregate results and create visualizations
Rscript aggregate_results.R results
```

### Option 2: Test Run (Small Subset)

```bash
# Run just 2 chunks with fewer iterations for testing
Rscript run_chunk.R --chunks 50 --chunk_id 1 --iters 5 --workers 2
Rscript run_chunk.R --chunks 50 --chunk_id 2 --iters 5 --workers 2
Rscript aggregate_results.R results
```

### Option 3: Interactive Analysis

```r
source("replicate_framework.R")

# Generate single scenario
grid <- make_full_grid()
scenario <- grid[1, ]  # EPV=3, event_frac=1/32, p=4, etc.

# Run one iteration
set.seed(12345)
data <- gen_dataset(p = scenario$p, event_frac = scenario$event_frac, 
                    EPV = scenario$EPV, noise_frac = scenario$noise_frac,
                    sparse = scenario$sparse, seed = 12345)
results <- eval_methods_once(data$Xtr, data$ytr, data$Xva, data$yva, data$piv, 42)
print(results)
```

---

## 📂 Repository Structure

```
.
├── replicate_framework.R    # Core simulation functions
├── run_chunk.R             # Parallel chunk execution
├── aggregate_results.R     # Results aggregation & visualization
├── results/                # Output directory (auto-created)
│   ├── sim_chunk_001.rds   # Raw results per chunk
│   ├── sim_chunk_*_rank_summary.csv  # Per-chunk summaries
│   ├── summary_ranks_overall.csv     # Final method rankings
│   ├── summary_by_EPV.csv  # Performance by EPV
│   ├── summary_by_p.csv    # Performance by # predictors
│   ├── simulation_report.txt # Comprehensive report
│   └── plots/              # Visualization outputs
│       ├── auc_by_method.png
│       ├── cal_slope_by_method.png
│       ├── brier_by_method.png
│       ├── performance_by_epv.png
│       └── performance_by_p.png
├── README.md               # This file
└── LICENSE                 # MIT license
```

---

## ⚙️ Command Line Options

### `run_chunk.R`
```bash
Options:
  --chunks INTEGER     Total chunks to split 1050 scenarios [default: 50]
  --chunk_id INTEGER   Which chunk to run (1-based) [default: 1]  
  --iters INTEGER      Iterations per scenario [default: 20]
  --outdir CHARACTER   Output directory [default: results]
  --workers INTEGER    Parallel workers [default: auto-detect]
```

### `aggregate_results.R`
```bash
Usage: Rscript aggregate_results.R [results_directory]
```

---

## 📈 Expected Outputs

### Primary Results Files

1. **`summary_ranks_overall.csv`** - Mean rankings across all scenarios
   ```csv
   method,mean_rank_auc,mean_rank_cals,mean_rank_brier,...
   Ridge,3.45,4.12,3.67,...
   LASSO,4.23,3.89,4.01,...
   ```

2. **`simulation_report.txt`** - Comprehensive summary with top performers

3. **Diagnostic Plots** - Box plots showing metric distributions by method

### Performance Summaries

- **By EPV**: How methods perform across different events-per-variable ratios
- **By Predictors**: Performance trends as dimensionality increases  
- **By Sparsity**: Impact of binary vs. continuous predictors

---

## 🔧 Computational Requirements

| Scale | Scenarios | Runtime* | Memory | Storage |
|-------|-----------|----------|---------|---------|
| **Test** | 50 | ~30 min | 2GB | 100MB |
| **Moderate** | 250 | ~2 hours | 4GB | 500MB |  
| **Complete** | 1,050 | ~8 hours | 8GB | 2GB |

*Approximate times on 8-core modern CPU with 20 iterations/scenario

### Parallelization Strategy

- **Chunk-level**: Split scenarios across chunks for embarrassing parallelism
- **Within-chunk**: Parallel processing within each chunk  
- **Fault-tolerant**: Failed chunks don't affect others
- **Resumable**: Can restart from any chunk

---

## 🎯 Key Implementation Features

### Robustness Enhancements

✅ **Data Standardization**: Proper scaling before sparse transformations  
✅ **Error Handling**: Graceful fallbacks when models fail  
✅ **Numerical Stability**: Winsorized calibration slopes, eigenvalue correction  
✅ **Reproducibility**: Comprehensive seed management  
✅ **Memory Efficiency**: Chunk-based processing with cleanup

### Methodological Fidelity

✅ **Exact Grid**: 1,050 scenarios matching original study  
✅ **Ranking Rules**: Identical rounding and tie-breaking procedures  
✅ **Cross-Validation**: Shared folds across methods for fair comparison  
✅ **Sample Sizes**: EPV-based training sizes, large validation sets  
✅ **Substitution Rules**: MLE fallback for failed specialized methods

---

## 🔍 Validation & Quality Assurance

### Automated Checks
- Grid size validation (must equal 1,050)
- Missing value detection and reporting
- Method convergence monitoring  
- File integrity verification
- Memory usage tracking

### Expected Findings
Based on original paper, expect:
1. **Ridge/LASSO** to outperform variance decomposition methods
2. **Better calibration** from penalization approaches
3. **Performance gaps** increasing with higher EPV
4. **Dimensionality effects** favoring penalization in moderate-p settings

---

## 📚 Citation & References

### Citing This Replication

```bibtex
@software{ribeiro2025replication,
  author = {Diogo Ribeiro},
  title = {Replication of Lohmann et al. (2023): Penalization vs. Variance Decomposition in Clinical Prediction Models},
  year = {2025},
  url = {[your-repo-url]},
  license = {MIT}
}
```

### Original Study

```bibtex
@article{lohmann2023comparison,
  title={Comparison of likelihood penalization and variance decomposition approaches for clinical prediction models: A simulation study},
  author={Lohmann, Lilli and Groenwold, Rolf HH and van Smeden, Maarten},
  journal={Biometrical Journal},
  volume={65},
  number={5},
  pages={e700193},
  year={2023},
  publisher={Wiley Online Library},
  doi={10.1002/bimj.202200108}
}
```

---

## 🤝 Contributing

Contributions welcome! Please:

1. **Fork** the repository
2. **Create** feature branch (`git checkout -b feature/improvement`)  
3. **Test** changes thoroughly
4. **Submit** pull request with clear description

### Areas for Enhancement
- Additional visualization options
- Alternative correlation structures  
- Extended method comparisons
- Computational optimizations

---

## ⚖️ License

MIT License - see `LICENSE` file for details.

---

## 📞 Support

- **Issues**: Use GitHub issue tracker
- **Questions**: Open discussion or contact maintainer
- **Documentation**: This README + inline code comments

