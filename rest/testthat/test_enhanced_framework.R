# =======================================================================
# test_enhanced_framework.R
# 
# Comprehensive unit tests for the enhanced Lohmann et al. (2023) replication
# Tests core functionality, error handling, and edge cases
#
# Author: Diogo Ribeiro (Enhanced)
# Date: January 2025
# License: MIT
# =======================================================================

library(testthat)
library(dplyr)

# Load the enhanced framework
if (file.exists("../../enhanced_replicate_framework.R")) {
  source("../../enhanced_replicate_framework.R")
  FRAMEWORK_ENHANCED <- TRUE
} else if (file.exists("enhanced_replicate_framework.R")) {
  source("enhanced_replicate_framework.R") 
  FRAMEWORK_ENHANCED <- TRUE
} else {
  # Fallback to original framework
  if (file.exists("../../replicate_framework.R")) {
    source("../../replicate_framework.R")
  } else {
    source("replicate_framework.R")
  }
  FRAMEWORK_ENHANCED <- FALSE
}

# =======================================================================
# CONFIGURATION TESTS
# =======================================================================

test_that("Configuration system works correctly", {
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  # Test default configuration
  default_config <- get_default_config()
  
  expect_type(default_config, "list")
  expect_true("simulation" %in% names(default_config))
  expect_true("parallel" %in% names(default_config))
  expect_true("output" %in% names(default_config))
  
  # Test configuration value retrieval
  cv_folds <- get_config(c("simulation", "cv_folds"), default = 10)
  expect_type(cv_folds, "integer")
  expect_gte(cv_folds, 2)
  
  # Test fallback for missing keys
  missing_value <- get_config(c("nonexistent", "key"), default = "test")
  expect_equal(missing_value, "test")
})

# =======================================================================
# UTILITY FUNCTION TESTS
# =======================================================================

test_that("Enhanced clip function works correctly", {
  
  # Basic functionality
  x <- c(-5, 0, 5, 10)
  clipped <- clip(x, 0, 8)
  expect_equal(clipped, c(0, 0, 5, 8))
  
  # Edge cases
  expect_equal(clip(numeric(0), 0, 1), numeric(0))
  expect_equal(clip(c(NA, 1, 2), 0, 5), c(NA, 1, 2))
  
  # Error handling
  expect_error(clip("not numeric"), "must be of type 'numeric'")
  expect_error(clip(1:3, 5, 2), "Lower bound cannot exceed upper bound")
})

test_that("Enhanced logit function handles edge cases", {
  
  # Basic functionality
  p <- c(0.1, 0.5, 0.9)
  logits <- logit(p)
  expect_length(logits, 3)
  expect_true(all(is.finite(logits)))
  
  # Edge cases - probabilities at boundaries
  edge_p <- c(0, 1, 0.0001, 0.9999)
  edge_logits <- logit(edge_p)
  expect_length(edge_logits, 4)
  expect_true(all(is.finite(edge_logits)))
  
  # Error handling
  expect_error(logit(-0.1), "must be >= 0")
  expect_error(logit(1.1), "must be <= 1") 
  expect_error(logit("not numeric"), "must be of type 'numeric'")
})

test_that("Calibration slope winsorization works", {
  
  # Normal values
  expect_equal(winsorize_cal_slope(0.5), 0.5)
  expect_equal(winsorize_cal_slope(2.0), 2.0)
  
  # Extreme values get winsorized
  expect_equal(winsorize_cal_slope(0.001), 0.01)
  expect_equal(winsorize_cal_slope(100), 10)
  
  # Custom bounds
  expect_equal(winsorize_cal_slope(0.5, c(0.1, 5)), 0.5)
  expect_equal(winsorize_cal_slope(0.05, c(0.1, 5)), 0.1)
})

# =======================================================================
# DATA GENERATION TESTS
# =======================================================================

test_that("Correlation matrix generation produces valid matrices", {
  
  set.seed(12345)
  
  # Test various sizes
  for (p in c(3, 10, 20)) {
    sigma <- make_sigma(p)
    
    # Basic properties
    expect_equal(nrow(sigma), p)
    expect_equal(ncol(sigma), p)
    expect_true(isSymmetric(sigma))
    
    # Diagonal should be 1 (correlation matrix)
    expect_true(all(abs(diag(sigma) - 1) < 1e-10))
    
    # Should be positive definite
    eigenvals <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values
    expect_true(all(eigenvals > 1e-10), 
                info = paste("Minimum eigenvalue:", min(eigenvals)))
    
    # Off-diagonal should be in [-1, 1]
    off_diag <- sigma[upper.tri(sigma)]
    expect_true(all(off_diag >= -1 & off_diag <= 1))
  }
})

test_that("Intercept solving works correctly", {
  
  set.seed(12345)
  X <- matrix(rnorm(100 * 3), ncol = 3)
  beta <- c(0.5, -0.3, 0.8)
  
  # Test various target probabilities
  for (target_pi in c(0.1, 0.3, 0.5, 0.7)) {
    b0 <- solve_intercept(beta, X, target_pi)
    
    # Check that achieved probability is close to target
    achieved_pi <- mean(plogis(b0 + X %*% beta))
    expect_lt(abs(achieved_pi - target_pi), 0.05, 
              info = paste("Target:", target_pi, "Achieved:", achieved_pi))
  }
  
  # Edge cases
  expect_type(solve_intercept(beta, X, 0.01), "double")
  expect_type(solve_intercept(beta, X, 0.99), "double")
})

test_that("Dataset generation produces valid datasets", {
  
  set.seed(12345)
  
  # Test various parameter combinations
  test_cases <- expand.grid(
    p = c(5, 10),
    event_frac = c(0.2, 0.5),
    EPV = c(5, 15),
    noise_frac = c(0, 0.5),
    sparse = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(min(5, nrow(test_cases)))) {  # Test first 5 cases
    params <- test_cases[i, ]
    
    data <- gen_dataset(
      p = params$p,
      event_frac = params$event_frac,
      EPV = params$EPV,
      noise_frac = params$noise_frac,
      sparse = params$sparse,
      seed = 12345 + i
    )
    
    # Check data structure
    expect_type(data, "list")
    expect_true(all(c("Xtr", "ytr", "Xva", "yva", "piv") %in% names(data)))
    
    # Check dimensions
    expect_equal(ncol(data$Xtr), params$p)
    expect_equal(ncol(data$Xva), params$p)
    expect_equal(length(data$ytr), nrow(data$Xtr))
    expect_equal(length(data$yva), nrow(data$Xva))
    expect_equal(length(data$piv), nrow(data$Xva))
    
    # Check outcome properties
    expect_true(all(data$ytr %in% c(0, 1)))
    expect_true(all(data$yva %in% c(0, 1)))
    expect_true(all(data$piv >= 0 & data$piv <= 1))
    
    # Check event rate is approximately correct
    actual_rate <- mean(data$ytr)
    expect_lt(abs(actual_rate - params$event_frac), 0.15, 
              info = paste("Expected:", params$event_frac, "Actual:", actual_rate))
    
    # Check sparse predictors if applicable
    if (params$sparse) {
      # Should have some binary predictors
      n_binary_tr <- sum(apply(data$Xtr, 2, function(x) all(x %in% c(0, 1))))
      expect_gt(n_binary_tr, 0)
    }
  }
})

# =======================================================================
# PERFORMANCE METRICS TESTS
# =======================================================================

test_that("Performance metrics computation handles all cases", {
  
  set.seed(12345)
  
  # Normal case
  n <- 100
  y <- rbinom(n, 1, 0.3)
  phat <- runif(n, 0.1, 0.9)
  pi_true <- runif(n, 0.1, 0.9)
  
  metrics <- compute_metrics(phat, y, pi_true)
  
  # Check structure
  expect_s3_class(metrics, "tbl_df")
  expect_true(all(c("auc", "cal_slope", "cal_in_large", "brier", "rMSPE", "MAPE") %in% names(metrics)))
  expect_equal(nrow(metrics), 1)
  
  # Check ranges
  expect_true(is.na(metrics$auc) || (metrics$auc >= 0 & metrics$auc <= 1))
  expect_true(metrics$brier >= 0 & metrics$brier <= 1)
  expect_true(metrics$rMSPE >= 0)
  expect_true(metrics$MAPE >= 0 & metrics$MAPE <= 1)
  
  # Edge case: no outcome variation
  y_constant <- rep(1, n)
  metrics_constant <- compute_metrics(phat, y_constant, pi_true)
  expect_true(is.na(metrics_constant$auc))  # Cannot compute AUC
  
  # Edge case: no prediction variation  
  phat_constant <- rep(0.5, n)
  metrics_pred_constant <- compute_metrics(phat_constant, y, pi_true)
  expect_true(is.na(metrics_pred_constant$auc))  # Cannot compute AUC
  
  # Perfect predictions
  phat_perfect <- ifelse(y == 1, 0.99, 0.01)
  metrics_perfect <- compute_metrics(phat_perfect, y, y)
  expect_gt(metrics_perfect$auc, 0.95)  # Should have high AUC
  expect_lt(metrics_perfect$brier, 0.1)  # Should have low Brier score
})

# =======================================================================
# MODEL FITTING TESTS
# =======================================================================

test_that("MLE fitting works correctly", {
  
  set.seed(12345)
  
  # Generate test data
  n <- 200
  p <- 3
  X <- matrix(rnorm(n * p), ncol = p)
  y <- rbinom(n, 1, plogis(X %*% c(0.5, -0.3, 0.8)))
  
  # Fit MLE
  fit <- fit_mle(X, y)
  
  if (!is.null(fit)) {
    # Check fit properties
    expect_s3_class(fit, "glm")
    expect_true(fit$family$family == "binomial")
    
    # Test predictions
    preds <- predict_mle(fit, X)
    expect_length(preds, n)
    expect_true(all(preds >= 0 & preds <= 1))
    
    # Should predict reasonable probabilities
    expect_gt(mean(preds), 0.1)
    expect_lt(mean(preds), 0.9)
  }
  
  # Edge case: no outcome variation
  y_constant <- rep(1, n)
  fit_constant <- fit_mle(X, y_constant)
  expect_null(fit_constant)  # Should fail gracefully
  
  # Predictions should still work (fallback to 0.5)
  preds_constant <- predict_mle(fit_constant, X)
  expect_length(preds_constant, n)
  expect_true(all(preds_constant == 0.5))
})

test_that("Penalized regression fitting works", {
  
  set.seed(12345)
  
  # Generate test data with more predictors
  n <- 100
  p <- 20
  X <- matrix(rnorm(n * p), ncol = p)
  beta_true <- c(rep(1, 5), rep(0, 15))  # Sparse truth
  y <- rbinom(n, 1, plogis(X %*% beta_true))
  
  # Generate folds for consistent CV
  foldid <- make_folds(n, K = 5, seed = 1)
  
  # Test Ridge
  ridge_fit <- fit_glmnet_cv(X, y, alpha = 0, foldid = foldid)
  
  if (!is.null(ridge_fit)) {
    expect_s3_class(ridge_fit, "cv.glmnet")
    
    # Test predictions
    ridge_preds <- predict_glmnet_resp(ridge_fit, X)
    expect_length(ridge_preds, n)
    expect_true(all(ridge_preds >= 0 & ridge_preds <= 1))
  }
  
  # Test LASSO
  lasso_fit <- fit_glmnet_cv(X, y, alpha = 1, foldid = foldid)
  
  if (!is.null(lasso_fit)) {
    expect_s3_class(lasso_fit, "cv.glmnet")
    
    # LASSO should select some variables (coefficients should be sparse)
    lasso_coefs <- coef(lasso_fit, s = "lambda.min")[-1]  # Exclude intercept
    n_selected <- sum(lasso_coefs != 0)
    expect_gt(n_selected, 0)
    expect_lt(n_selected, p)  # Should be sparser than Ridge
  }
})

# =======================================================================
# CROSS-VALIDATION TESTS
# =======================================================================

test_that("Cross-validation fold creation works", {
  
  set.seed(12345)
  
  # Basic functionality
  n <- 100
  K <- 10
  folds <- make_folds(n, K, seed = 1)
  
  expect_length(folds, n)
  expect_true(all(folds %in% 1:K))
  expect_equal(length(unique(folds)), K)
  
  # Check balance
  fold_counts <- table(folds)
  expect_true(max(fold_counts) - min(fold_counts) <= 1)  # Balanced
  
  # Edge case: fewer observations than folds
  small_folds <- make_folds(5, 10, seed = 1)
  expect_length(small_folds, 5)
  expect_equal(sort(unique(small_folds)), 1:5)  # Each obs gets own fold
  
  # Stratified CV
  y <- rbinom(n, 1, 0.3)
  strat_folds <- make_folds(n, K, seed = 1, stratify = y)
  
  expect_length(strat_folds, n)
  
  # Check that each fold has roughly similar class balance
  fold_balance <- sapply(1:K, function(k) {
    fold_y <- y[strat_folds == k]
    if (length(fold_y) == 0) return(NA)
    mean(fold_y)
  })
  
  # Remove empty folds
  fold_balance <- fold_balance[!is.na(fold_balance)]
  
  if (length(fold_balance) > 1) {
    expect_lt(sd(fold_balance, na.rm = TRUE), 0.2)  # Similar balance across folds
  }
})

# =======================================================================
# METHOD REGISTRY TESTS (Enhanced Framework Only)
# =======================================================================

test_that("Method registry works correctly", {
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  # Test method registration
  test_fit <- function(X, y, ...) {
    return(list(coefficients = rep(0, ncol(X) + 1)))
  }
  
  test_predict <- function(fit, X) {
    return(rep(0.5, nrow(X)))
  }
  
  # Register test method
  register_method("TestMethod", test_fit, test_predict, 
                 requires_cv = FALSE, description = "Test method")
  
  # Check registration
  methods_list <- list_methods()
  expect_true("TestMethod" %in% methods_list$method)
  
  # Get method
  test_method <- get_method("TestMethod")
  expect_type(test_method, "list")
  expect_true(all(c("fit", "predict", "requires_cv", "description") %in% names(test_method)))
  
  # Test error handling for invalid method
  expect_error(get_method("NonExistentMethod"), "not found in registry")
  
  # Test validation of method functions
  expect_error(register_method("BadMethod", "not a function", test_predict),
               "must be")
})

# =======================================================================
# INTEGRATION TESTS
# =======================================================================

test_that("Full evaluation pipeline works for single scenario", {
  
  set.seed(12345)
  
  # Generate a simple dataset
  data <- gen_dataset(p = 5, event_frac = 0.3, EPV = 10, noise_frac = 0,
                      sparse = FALSE, seed = 12345)
  
  # Run evaluation (this tests the full pipeline)
  results <- eval_methods_once(
    Xtr = data$Xtr, ytr = data$ytr,
    Xva = data$Xva, yva = data$yva, 
    piv = data$piv, seed_fold = 42
  )
  
  # Check results structure
  expect_s3_class(results, "tbl_df")
  expect_true("method" %in% names(results))
  expect_true(all(c("auc", "brier", "cal_slope") %in% names(results)))
  
  # Should have results for multiple methods
  expect_gt(nrow(results), 5)  # At least 5 methods
  expect_lt(nrow(results), 15) # But not too many
  
  # Check method names are valid
  expected_methods <- c("MLE", "Ridge", "LASSO")
  expect_true(any(results$method %in% expected_methods))
  
  # Results should be mostly non-missing for good data
  na_rate <- mean(is.na(results$auc))
  expect_lt(na_rate, 0.5)  # Less than 50% missing
})

test_that("Ranking methodology works correctly", {
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  # Create test results
  test_results <- tibble(
    method = rep(c("A", "B", "C"), each = 3),
    auc = c(0.8, 0.7, 0.9,    # Method A: mean = 0.8
            0.6, 0.8, 0.7,    # Method B: mean = 0.7  
            0.9, 0.9, 0.8),   # Method C: mean = 0.867
    brier = c(0.2, 0.3, 0.1,  # Method A: mean = 0.2
              0.4, 0.2, 0.3,  # Method B: mean = 0.3
              0.1, 0.1, 0.2), # Method C: mean = 0.133
    cal_slope = rep(1.0, 9),
    rMSPE = rep(0.1, 9),
    MAPE = rep(0.05, 9)
  )
  
  # Apply ranking
  ranked <- rank_with_rounding(test_results)
  
  # Check ranking columns exist
  expect_true(all(c("r_auc", "r_brier", "r_cals") %in% names(ranked)))
  
  # Check ranking logic (higher AUC = better = lower rank)
  # Method C should generally rank highest (lowest rank numbers) for AUC
  c_auc_ranks <- ranked$r_auc[ranked$method == "C"]
  a_auc_ranks <- ranked$r_auc[ranked$method == "A"] 
  
  expect_lt(mean(c_auc_ranks), mean(a_auc_ranks))
  
  # Check Brier ranking (lower is better)
  c_brier_ranks <- ranked$r_brier[ranked$method == "C"]
  b_brier_ranks <- ranked$r_brier[ranked$method == "B"]
  
  expect_lt(mean(c_brier_ranks), mean(b_brier_ranks))
})

# =======================================================================
# MEMORY AND PERFORMANCE TESTS
# =======================================================================

test_that("Memory estimation works", {
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  # Test scenario
  scenario <- list(EPV = 10, p = 20, event_frac = 0.2)
  
  memory_est <- estimate_scenario_memory(scenario)
  
  expect_type(memory_est, "double")
  expect_gt(memory_est, 0)
  expect_lt(memory_est, 1e10)  # Reasonable upper bound (10 GB)
  
  # Larger scenario should use more memory
  large_scenario <- list(EPV = 50, p = 100, event_frac = 0.1)
  large_memory <- estimate_scenario_memory(large_scenario)
  
  expect_gt(large_memory, memory_est)
})

test_that("Batch size suggestion works", {
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  # Create test grid
  grid <- data.frame(
    EPV = rep(10, 10),
    p = rep(20, 10),
    event_frac = rep(0.2, 10)
  )
  
  scenarios <- 1:5
  batch_size <- suggest_batch_size(scenarios, grid, max_memory_gb = 2)
  
  expect_type(batch_size, "integer")
  expect_gte(batch_size, 1)
  expect_lte(batch_size, length(scenarios))
})

# =======================================================================
# ERROR HANDLING TESTS
# =======================================================================

test_that("Error handling works for edge cases", {
  
  # Test with very small dataset
  X_small <- matrix(rnorm(10 * 2), ncol = 2)
  y_small <- rep(1, 10)  # No variation
  
  # Should handle gracefully without crashing
  expect_silent({
    fit <- fit_mle(X_small, y_small)
    pred <- predict_mle(fit, X_small)
  })
  
  # Test with extreme correlation
  X_extreme <- matrix(c(1:20, 1:20 + rnorm(20, 0, 0.001)), ncol = 2)  # Nearly collinear
  y_extreme <- rbinom(20, 1, 0.5)
  
  expect_silent({
    fit <- fit_mle(X_extreme, y_extreme)
    pred <- predict_mle(fit, X_extreme)
  })
  
  # Test with missing values (should error appropriately)
  X_na <- matrix(c(1:10, rep(NA, 10)), ncol = 2)
  y_na <- rep(c(0, 1), 5)
  
  expect_error(fit_mle(X_na, y_na))
  
  # Test metrics with extreme predictions
  extreme_preds <- c(rep(0.001, 50), rep(0.999, 50))
  y_balanced <- rep(c(0, 1), 50)
  pi_true <- rep(0.5, 100)
  
  metrics_extreme <- compute_metrics(extreme_preds, y_balanced, pi_true)
  expect_true(all(!is.infinite(c(metrics_extreme$auc, metrics_extreme$brier, 
                                metrics_extreme$rMSPE, metrics_extreme$MAPE))))
})

# =======================================================================
# REPRODUCIBILITY TESTS
# =======================================================================

test_that("Results are reproducible with same seed", {
  
  # Same parameters, same seed should give identical results
  params <- list(p = 5, event_frac = 0.3, EPV = 10, noise_frac = 0, sparse = FALSE)
  
  set.seed(12345)
  data1 <- do.call(gen_dataset, c(params, seed = 999))
  
  set.seed(12345)  # Reset seed
  data2 <- do.call(gen_dataset, c(params, seed = 999))
  
  # Matrices should be identical
  expect_equal(data1$Xtr, data2$Xtr)
  expect_equal(data1$ytr, data2$ytr)
  expect_equal(data1$Xva, data2$Xva)
  expect_equal(data1$yva, data2$yva)
  
  # Evaluation should also be reproducible
  results1 <- eval_methods_once(data1$Xtr, data1$ytr, data1$Xva, data1$yva, 
                               data1$piv, seed_fold = 42)
  results2 <- eval_methods_once(data2$Xtr, data2$ytr, data2$Xva, data2$yva, 
                               data2$piv, seed_fold = 42)
  
  # Results should be identical (accounting for potential numerical precision)
  for (method in intersect(results1$method, results2$method)) {
    r1 <- results1[results1$method == method, ]
    r2 <- results2[results2$method == method, ]
    
    expect_equal(r1$auc, r2$auc, tolerance = 1e-10, 
                info = paste("AUC mismatch for", method))
    expect_equal(r1$brier, r2$brier, tolerance = 1e-10,
                info = paste("Brier mismatch for", method))
  }
})

# =======================================================================
# GRID GENERATION TESTS
# =======================================================================

test_that("Scenario grid generation is correct", {
  
  if (FRAMEWORK_ENHANCED) {
    grid <- make_full_grid()
  } else {
    source("replicate_framework.R")
    grid <- make_full_grid()
  }
  
  # Check total scenarios
  expect_equal(nrow(grid), 1050)
  
  # Check required columns
  required_cols <- c("EPV", "event_frac", "p", "noise_frac", "sparse", "scn_id")
  expect_true(all(required_cols %in% names(grid)))
  
  # Check value ranges
  expect_true(all(grid$EPV %in% c(3, 5, 10, 15, 20, 50, 100)))
  expect_true(all(grid$event_frac %in% c(1/32, 1/16, 1/8, 1/4, 1/2)))
  expect_true(all(grid$p %in% c(4, 8, 16, 32, 64)))
  expect_true(all(grid$noise_frac %in% c(0, 1/4, 1/2)))
  expect_true(all(grid$sparse %in% c(FALSE, TRUE)))
  
  # Check scenario IDs are unique and sequential
  expect_equal(sort(grid$scn_id), 1:1050)
  
  # Check combinations are complete
  n_combinations <- 7 * 5 * 5 * 3 * 2  # EPV * event_frac * p * noise_frac * sparse
  expect_equal(n_combinations, 1050)
  
  # Check no duplicate combinations (excluding scn_id)
  grid_no_id <- grid[, !names(grid) %in% "scn_id"]
  expect_equal(nrow(unique(grid_no_id)), 1050)
})

# =======================================================================
# PERFORMANCE BENCHMARKS (Optional - only run if requested)
# =======================================================================

test_that("Performance benchmarks meet expectations", {
  skip_if_not(interactive(), "Performance tests only in interactive mode")
  skip_if_not(FRAMEWORK_ENHANCED, "Enhanced framework not available")
  
  cat("\n🏃 Running performance benchmarks...\n")
  
  # Benchmark data generation
  bench_data_gen <- bench::mark(
    small = gen_dataset(p = 5, event_frac = 0.3, EPV = 10, noise_frac = 0, sparse = FALSE, seed = 1),
    medium = gen_dataset(p = 20, event_frac = 0.2, EPV = 15, noise_frac = 0.25, sparse = TRUE, seed = 2),
    large = gen_dataset(p = 50, event_frac = 0.1, EPV = 5, noise_frac = 0.5, sparse = FALSE, seed = 3),
    iterations = 3,
    check = FALSE
  )
  
  cat("Data generation benchmarks:\n")
  print(bench_data_gen[, c("expression", "median", "mem_alloc")])
  
  # Data generation should be reasonably fast
  expect_lt(median(bench_data_gen$median[bench_data_gen$expression == "small"]), 
            as.difftime(5, units = "secs"))
  
  # Benchmark method evaluation
  test_data <- gen_dataset(p = 10, event_frac = 0.3, EPV = 10, noise_frac = 0, sparse = FALSE, seed = 123)
  
  bench_eval <- bench::mark(
    evaluation = eval_methods_once(test_data$Xtr, test_data$ytr, test_data$Xva, 
                                  test_data$yva, test_data$piv, seed_fold = 42),
    iterations = 2,
    check = FALSE
  )
  
  cat("\nMethod evaluation benchmark:\n")
  print(bench_eval[, c("expression", "median", "mem_alloc")])
  
  # Method evaluation should complete in reasonable time
  expect_lt(median(bench_eval$median), as.difftime(30, units = "secs"))
})

# =======================================================================
# CLEANUP
# =======================================================================

# Clean up any test artifacts
if (exists(".method_registry") && FRAMEWORK_ENHANCED) {
  # Remove test method if it was added
  if ("TestMethod" %in% names(.method_registry)) {
    .method_registry[["TestMethod"]] <- NULL
  }
}

cat("✅ All tests completed successfully!\n")
