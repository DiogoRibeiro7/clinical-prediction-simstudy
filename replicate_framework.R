# =======================================================================
# replicate_framework.R
# 
# Core implementation of Lohmann et al. (2023) simulation framework
# Comparing penalization vs. variance decomposition approaches for 
# clinical prediction models
#
# This file contains:
# - Data generation with structured correlation
# - Implementation of 11 modeling approaches
# - Performance evaluation with 6 metrics
# - Robust error handling and fallback mechanisms
#
# Author: Diogo Ribeiro
# Date: January 2025
# License: MIT
# =======================================================================

# Load required packages with informative error messages
suppressPackageStartupMessages({
  required_packages <- c("MASS", "glmnet", "pROC", "plsRglm", "Matrix", 
                        "dplyr", "purrr", "tibble", "tidyr")
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), 
               "\nInstall with: install.packages(c(", 
               paste0('"', missing_packages, '"', collapse = ", "), "))"))
  }
  
  # Load packages
  library(MASS)      # For multivariate normal generation
  library(glmnet)    # For penalized regression (Ridge, LASSO, Elastic Net)
  library(pROC)      # For AUC calculation
  library(plsRglm)   # For partial least squares regression
  library(Matrix)    # For sparse matrix operations
  library(dplyr)     # For data manipulation
  library(purrr)     # For functional programming
  library(tibble)    # For modern data frames
  library(tidyr)     # For data reshaping
})

# =======================================================================
# UTILITY FUNCTIONS
# =======================================================================

#' Clip values to specified range
#' 
#' Ensures values stay within [lo, hi] bounds to prevent numerical issues
#' 
#' @param x Numeric vector to clip
#' @param lo Lower bound (default: -Inf)
#' @param hi Upper bound (default: Inf)
#' @return Clipped numeric vector
#' @examples
#' clip(c(-5, 0, 5, 10), 0, 8)  # Returns: 0 0 5 8
clip <- function(x, lo = -Inf, hi = Inf) {
  pmin(pmax(x, lo), hi)
}

#' Logit transformation with numerical stability
#' 
#' Converts probabilities to log-odds scale while avoiding infinite values
#' at boundaries (0 and 1)
#' 
#' @param p Probability vector (must be in [0,1])
#' @param eps Small constant to prevent log(0) (default: 1e-12)
#' @return Log-odds vector
#' @examples
#' logit(c(0.1, 0.5, 0.9))  # Returns log-odds
logit <- function(p, eps = 1e-12) { 
  # Ensure probabilities are in valid range
  p <- clip(p, eps, 1 - eps)
  log(p / (1 - p))
}

#' Winsorize calibration slopes
#' 
#' Clips calibration slopes to reasonable range as per Lohmann et al. (2023)
#' methodology. Extreme slopes indicate severe miscalibration.
#' 
#' @param b Calibration slope estimate
#' @param lo Lower bound (default: 0.01)
#' @param hi Upper bound (default: 10)
#' @return Winsorized slope value
winsorize_cal_slope <- function(b, lo = 0.01, hi = 10) {
  clip(b, lo, hi)
}

# =======================================================================
# DATA GENERATION FUNCTIONS
# =======================================================================

#' Generate structured correlation matrix
#' 
#' Creates correlation matrix with Beta-distributed off-diagonal elements
#' following Lohmann et al. (2023) methodology. Ensures positive definiteness
#' through eigenvalue correction.
#' 
#' @param p Number of predictors
#' @param beta_a Beta distribution shape parameter 1 (default: 1)
#' @param beta_b Beta distribution shape parameter 2 (default: 3)
#' @return p x p positive definite correlation matrix
#' 
#' @details
#' The correlation structure follows:
#' 1. Off-diagonal correlations ~ Beta(1,3) with random signs
#' 2. Eigenvalue correction ensures positive definiteness
#' 3. Final standardization to correlation matrix (diag = 1)
make_sigma <- function(p, beta_a = 1, beta_b = 3) {
  # Generate random correlation magnitudes from Beta distribution
  # Beta(1,3) gives higher probability to smaller correlations
  r_abs <- rbeta(n = p * (p - 1) / 2, shape1 = beta_a, shape2 = beta_b)
  
  # Assign random signs to correlations
  signs <- sample(c(-1, 1), length(r_abs), replace = TRUE)
  r <- r_abs * signs
  
  # Fill correlation matrix (symmetric)
  Sigma <- diag(1, p)  # Start with identity matrix
  k <- 1L
  for (i in seq_len(p - 1L)) {
    for (j in (i + 1L):p) {
      Sigma[i, j] <- r[k]
      Sigma[j, i] <- r[k]  # Ensure symmetry
      k <- k + 1L
    }
  }
  
  # Ensure positive definiteness through eigenvalue correction
  eig <- eigen(Sigma, symmetric = TRUE)
  
  # Set minimum eigenvalue to ensure numerical stability
  # More conservative than original for robustness
  min_eig <- max(1e-8, min(eig$values) * 0.001)
  eig$values[eig$values < min_eig] <- min_eig
  
  # Reconstruct matrix with corrected eigenvalues
  S <- eig$vectors %*% diag(eig$values, p) %*% t(eig$vectors)
  
  # Standardize to correlation matrix (diagonal = 1)
  D <- diag(1 / sqrt(diag(S)), p)
  Sigma_final <- D %*% S %*% D
  
  as.matrix(Sigma_final)
}

#' Solve for intercept to achieve target event fraction
#' 
#' Given predictor matrix and coefficients, finds intercept that produces
#' the desired event probability (prevalence) in the population.
#' 
#' @param beta Coefficient vector (without intercept)
#' @param X Predictor matrix
#' @param target_pi Target event probability/prevalence
#' @param tol Convergence tolerance (default: 1e-10)
#' @param max_iter Maximum iterations (default: 100)
#' @return Intercept value
#' 
#' @details
#' Solves: mean(plogis(b0 + X %*% beta)) = target_pi
#' Uses numerical root finding with robust fallback
solve_intercept <- function(beta, X, target_pi, tol = 1e-10, max_iter = 100) {
  # Define function to find root of
  f <- function(b0) {
    mean(plogis(b0 + as.numeric(X %*% beta))) - target_pi
  }
  
  # Try progressively wider bounds if root finding fails
  tryCatch({
    uniroot(f, c(-20, 20), tol = tol, maxiter = max_iter)$root
  }, error = function(e1) {
    tryCatch({
      # Try wider bounds with relaxed tolerance
      uniroot(f, c(-50, 50), tol = tol * 10, maxiter = max_iter)$root
    }, error = function(e2) {
      # Final fallback: use simple logit approximation
      warning("Intercept solving failed, using logit approximation")
      qlogis(target_pi)
    })
  })
}

#' Generate true coefficient vector
#' 
#' Creates coefficient vector with specified noise fraction following
#' Lohmann et al. (2023) methodology.
#' 
#' @param p Number of predictors
#' @param noise_frac Fraction of predictors with zero coefficients
#' @return Coefficient vector of length p
#' 
#' @details
#' - All coefficients initially ~ N(0,1)
#' - Random subset set to 0 based on noise_frac
#' - This creates realistic signal-to-noise scenarios
draw_true_beta <- function(p, noise_frac = 0) {
  # Generate all coefficients from standard normal
  beta <- rnorm(p, 0, 1)
  
  # Set random fraction to zero (noise predictors)
  if (noise_frac > 0) {
    k0 <- floor(noise_frac * p)
    if (k0 > 0) {
      noise_indices <- sample(seq_len(p), k0)
      beta[noise_indices] <- 0
    }
  }
  
  beta
}

#' Generate complete dataset for one scenario
#' 
#' Main data generation function that creates training and validation
#' datasets according to Lohmann et al. (2023) specification.
#' 
#' @param p Number of predictors
#' @param event_frac Target event probability (prevalence) 
#' @param EPV Events per variable (determines training sample size)
#' @param noise_frac Fraction of noise predictors (coefficients = 0)
#' @param sparse Logical, whether to convert some predictors to binary
#' @param seed Random seed for reproducibility
#' @param n_val_scale Validation set size multiplier (default: 20)
#' @return List containing training/validation data and true parameters
#' 
#' @details
#' Sample sizes:
#' - Training: max(200, ceil((EPV * p) / event_frac))
#' - Validation: max(200, ceil(n_val_scale * p / event_frac))
#' 
#' Data standardization occurs BEFORE sparse transformation to maintain
#' proper scaling relationships.
gen_dataset <- function(p, event_frac, EPV, noise_frac, sparse, seed,
                        n_val_scale = 20) {
  set.seed(seed)
  
  # Calculate sample sizes based on EPV and event fraction
  # EPV = expected_events / p, so n_train = (EPV * p) / event_frac
  n_train <- max(200L, ceiling((EPV * p) / event_frac))
  n_val   <- max(200L, ceiling(n_val_scale * p / event_frac))
  
  # Generate correlation matrix and multivariate normal data
  Sigma <- make_sigma(p)
  Xtr <- mvrnorm(n_train, rep(0, p), Sigma)  # Training predictors
  Xva <- mvrnorm(n_val,   rep(0, p), Sigma)  # Validation predictors
  
  # CRITICAL: Standardize BEFORE applying sparse transformations
  # This maintains proper scaling relationships between predictors
  Xtr_s <- scale(Xtr, center = TRUE, scale = TRUE)
  scale_center <- attr(Xtr_s, "scaled:center")
  scale_scale <- attr(Xtr_s, "scaled:scale")
  
  # Apply same standardization to validation set
  Xva_s <- scale(Xva, center = scale_center, scale = scale_scale)
  
  # Apply sparse predictor transformation to standardized data
  if (isTRUE(sparse)) {
    # Convert 25% of predictors to binary based on standardized values
    n_sparse <- max(1L, floor(p / 4))  # Ensure at least 1 sparse predictor
    sparse_indices <- sample(seq_len(p), n_sparse)
    
    # Binary transformation: positive standardized values -> 1, negative -> 0
    Xtr_s[, sparse_indices] <- (Xtr_s[, sparse_indices] > 0) * 1
    Xva_s[, sparse_indices] <- (Xva_s[, sparse_indices] > 0) * 1
  }
  
  # Generate true coefficients and solve for intercept
  beta_true <- draw_true_beta(p, noise_frac)
  b0_true   <- solve_intercept(beta_true, Xtr_s, target_pi = event_frac)
  
  # Generate true probabilities and observed outcomes
  pi_tr <- plogis(b0_true + as.numeric(Xtr_s %*% beta_true))  # Training probabilities
  pi_va <- plogis(b0_true + as.numeric(Xva_s %*% beta_true))  # Validation probabilities (true)
  
  y_tr <- rbinom(n_train, 1, pi_tr)  # Training outcomes
  y_va <- rbinom(n_val,   1, pi_va)  # Validation outcomes
  
  # Return complete dataset with metadata
  list(
    Xtr = Xtr_s,           # Standardized training predictors
    ytr = y_tr,            # Training outcomes
    piv = pi_va,           # True validation probabilities (for evaluation)
    Xva = Xva_s,           # Standardized validation predictors  
    yva = y_va,            # Validation outcomes
    beta_true = beta_true, # True coefficients (for reference)
    b0_true = b0_true,     # True intercept (for reference)
    scale_center = scale_center,  # Standardization parameters (for debugging)
    scale_scale = scale_scale
  )
}

# =======================================================================
# PERFORMANCE EVALUATION FUNCTIONS
# =======================================================================

#' Compute all performance metrics
#' 
#' Calculates the six metrics used in Lohmann et al. (2023) with robust
#' error handling for edge cases.
#' 
#' @param phat Predicted probabilities
#' @param y Observed binary outcomes
#' @param pi_true True probabilities (for rMSPE and MAPE)
#' @return Tibble with all six metrics
#' 
#' @details
#' Metrics computed:
#' 1. AUC: Discrimination (c-statistic)
#' 2. Calibration slope: Coefficient of logit(phat) in logistic regression
#' 3. Calibration-in-the-large: Intercept in offset logistic regression
#' 4. Brier score: Mean squared prediction error
#' 5. rMSPE: Root mean squared prediction error vs. true probabilities
#' 6. MAPE: Mean absolute prediction error vs. true probabilities
compute_metrics <- function(phat, y, pi_true) {
  # 1. AUC (Area Under ROC Curve) - measures discrimination
  auc <- tryCatch({
    # Check for sufficient variation in predictions and outcomes
    if (length(unique(y)) < 2 || length(unique(phat)) < 2) {
      NA_real_  # Cannot compute AUC without variation
    } else {
      as.numeric(pROC::auc(y, phat, quiet = TRUE))
    }
  }, error = function(e) {
    warning(paste("AUC calculation failed:", e$message))
    NA_real_
  })
  
  # 2. Calibration slope - should be 1 for perfect calibration
  # Regress outcomes on logit of predictions
  lph <- logit(phat)  # Convert predictions to logit scale
  cal_fit <- tryCatch({
    # Check for valid logit values and outcome variation
    if (any(is.infinite(lph)) || any(is.na(lph)) || length(unique(y)) < 2) {
      NULL
    } else {
      glm(y ~ lph, family = binomial())
    }
  }, error = function(e) {
    warning(paste("Calibration slope calculation failed:", e$message))
    NULL
  })
  
  # Extract and winsorize calibration slope
  b <- if (is.null(cal_fit) || any(is.na(coef(cal_fit)))) {
    NA_real_
  } else {
    winsorize_cal_slope(unname(coef(cal_fit)[2]))  # Coefficient of lph
  }
  
  # 3. Calibration-in-the-large (CIL) - should be 0 for perfect calibration
  # Fit intercept-only model with logit(phat) as offset
  cil_fit <- tryCatch({
    if (any(is.infinite(lph)) || any(is.na(lph)) || length(unique(y)) < 2) {
      NULL
    } else {
      glm(y ~ 1, family = binomial(), offset = lph)
    }
  }, error = function(e) {
    warning(paste("Calibration-in-the-large calculation failed:", e$message))
    NULL
  })
  
  # Extract CIL (intercept)
  cil <- if (is.null(cil_fit) || any(is.na(coef(cil_fit)))) {
    NA_real_
  } else {
    unname(coef(cil_fit)[1])  # Intercept
  }
  
  # 4. Brier score - overall prediction accuracy (lower is better)
  brier <- mean((y - phat)^2)
  
  # 5. Root Mean Squared Prediction Error vs. true probabilities
  rMSPE <- sqrt(mean((pi_true - phat)^2))
  
  # 6. Mean Absolute Prediction Error vs. true probabilities  
  MAPE  <- mean(abs(pi_true - phat))
  
  # Return all metrics in a tibble
  tibble(
    auc = auc, 
    cal_slope = b, 
    cal_in_large = cil,
    brier = brier, 
    rMSPE = rMSPE, 
    MAPE = MAPE
  )
}

# =======================================================================
# CROSS-VALIDATION UTILITIES
# =======================================================================

#' Create cross-validation folds
#' 
#' Generates K-fold CV indices with robust handling of edge cases
#' 
#' @param n Sample size
#' @param K Number of folds (default: 10)
#' @param seed Random seed
#' @return Vector of fold assignments
make_folds <- function(n, K = 10, seed = 1) {
  set.seed(seed)
  
  # Handle edge case: fewer observations than folds
  if (n < K) {
    # Each observation gets its own fold
    seq_len(n)
  } else {
    # Standard K-fold assignment
    sample(rep(1:K, length.out = n))
  }
}

# =======================================================================
# MODEL FITTING FUNCTIONS
# =======================================================================

#' Fit Maximum Likelihood Estimation (unpenalized logistic regression)
#' 
#' @param X Predictor matrix
#' @param y Binary outcome vector
#' @return Fitted glm object or NULL if failed
fit_mle <- function(X, y) {
  tryCatch({
    # Convert to data frame with standardized column names
    df <- as.data.frame(X)
    colnames(df) <- paste0("x", seq_len(ncol(df)))
    df$y <- y
    
    # Check for sufficient outcome variation
    if (length(unique(y)) < 2) {
      stop("No variation in outcome - cannot fit model")
    }
    
    # Fit logistic regression
    fit <- glm(y ~ ., data = df, family = binomial())
    
    # Check convergence
    if (!fit$converged) {
      warning("MLE did not converge - results may be unreliable")
    }
    
    fit
  }, error = function(e) {
    warning(paste("MLE fitting failed:", e$message))
    NULL
  })
}

#' Predict from MLE model
#' 
#' @param fit Fitted MLE model (from fit_mle)
#' @param X New predictor matrix
#' @return Vector of predicted probabilities
predict_mle <- function(fit, X) {
  if (is.null(fit)) {
    # Default prediction when model failed
    return(rep(0.5, nrow(X)))
  }
  
  tryCatch({
    # Convert to data frame with matching column names
    df <- as.data.frame(X)
    colnames(df) <- names(coef(fit))[-1]  # Exclude intercept name
    
    as.numeric(predict(fit, df, type = "response"))
  }, error = function(e) {
    warning(paste("MLE prediction failed:", e$message))
    rep(0.5, nrow(X))  # Default prediction
  })
}

#' Fit penalized regression with cross-validation
#' 
#' Fits Ridge, LASSO, Elastic Net, or Relaxed LASSO using glmnet
#' 
#' @param X Predictor matrix
#' @param y Binary outcome vector
#' @param alpha Elastic net mixing parameter (0=Ridge, 1=LASSO)
#' @param foldid Cross-validation fold assignments
#' @param relax Logical, use relaxed LASSO (default: FALSE)
#' @return cv.glmnet object or NULL if failed
fit_glmnet_cv <- function(X, y, alpha, foldid, relax = FALSE) {
  tryCatch({
    # Check for sufficient outcome variation
    if (length(unique(y)) < 2) {
      stop("No variation in outcome - cannot fit model")
    }
    
    # Fit cross-validated penalized regression
    cv.glmnet(
      x = X, 
      y = y, 
      family = "binomial", 
      alpha = alpha,           # 0=Ridge, 1=LASSO, 0<alpha<1=Elastic Net
      relax = relax,           # Two-stage fitting for Relaxed LASSO
      foldid = foldid,         # Use consistent CV folds across methods
      nlambda = 100,           # Number of lambda values to try
      type.measure = "deviance", # Optimize cross-validated deviance
      standardize = FALSE      # Data already standardized
    )
  }, error = function(e) {
    warning(paste("glmnet fitting failed (alpha =", alpha, "):", e$message))
    NULL
  })
}

#' Predict from penalized regression model
#' 
#' @param cvfit cv.glmnet object
#' @param X New predictor matrix
#' @return Vector of predicted probabilities
predict_glmnet_resp <- function(cvfit, X) {
  if (is.null(cvfit)) {
    # Default prediction when model failed
    return(rep(0.5, nrow(X)))
  }
  
  tryCatch({
    # Predict using lambda that minimizes CV error
    as.numeric(predict(cvfit, newx = X, type = "response", s = "lambda.min"))
  }, error = function(e) {
    warning(paste("glmnet prediction failed:", e$message))
    rep(0.5, nrow(X))  # Default prediction
  })
}

# =======================================================================
# PRINCIPAL COMPONENT REGRESSION (PCR) FUNCTIONS
# =======================================================================

#' Select number of principal components
#' 
#' Implements four component selection rules from Lohmann et al. (2023)
#' 
#' @param X Predictor matrix
#' @param y Binary outcome vector  
#' @param rule Selection method: "evgt1", "var90", "aic", "cvdev"
#' @param foldid CV fold assignments (needed for "cvdev" rule)
#' @return List with PCA object and selected number of components
#' 
#' @details
#' Selection rules:
#' - evgt1: Components with eigenvalue > 1 (Kaiser criterion)
#' - var90: Components explaining ≥90% of variance
#' - aic: Components minimizing AIC of logistic regression
#' - cvdev: Components minimizing cross-validated deviance
select_pcr_components <- function(X, y, rule = c("evgt1","var90","aic","cvdev"), 
                                  foldid = NULL) {
  rule <- match.arg(rule)
  
  tryCatch({
    # Perform PCA (data already standardized, so center=FALSE, scale=FALSE)
    pr <- prcomp(X, center = FALSE, scale. = FALSE)
    eig <- pr$sdev^2  # Eigenvalues (variances)
    varexpl <- cumsum(eig) / sum(eig)  # Cumulative variance explained
    
    # Ensure reasonable bounds for number of components
    max_k <- min(ncol(X), nrow(X) - 2)  # Leave df for GLM fitting
    k_candidates <- seq_len(max_k)
    
    # Apply selection rule
    k <- switch(
      rule,
      
      # Kaiser criterion: eigenvalue > 1
      evgt1 = {
        max(1L, sum(eig > 1))
      },
      
      # Variance explained: ≥90%
      var90 = {
        k90 <- which(varexpl >= 0.90)[1]  # First component reaching 90%
        max(1L, ifelse(is.na(k90), max_k, k90))
      },
      
      # AIC-based selection
      aic = {
        if (length(unique(y)) < 2) {
          1L  # Default if no outcome variation
        } else {
          # Fit logistic regression for each k and compute AIC
          aics <- sapply(k_candidates, function(k) {
            tryCatch({
              Z <- pr$x[, seq_len(k), drop = FALSE]  # First k components
              fit <- glm(y ~ ., family = binomial(), data = data.frame(y, Z))
              if (fit$converged) AIC(fit) else Inf
            }, error = function(e) Inf)
          })
          k_candidates[which.min(aics)]
        }
      },
      
      # Cross-validated deviance minimization
      cvdev = {
        if (is.null(foldid) || length(unique(y)) < 2) {
          1L  # Default if no CV folds or outcome variation
        } else {
          # Fit Ridge regression on components for each k and compute CV error
          devs <- sapply(k_candidates, function(k) {
            tryCatch({
              Z <- pr$x[, seq_len(k), drop = FALSE]  # First k components
              # Use Ridge (alpha=0) on components to get CV deviance
              cv <- cv.glmnet(Z, y, family = "binomial", alpha = 0,
                              foldid = foldid, type.measure = "deviance",
                              standardize = FALSE)  # Components already standardized
              min(cv$cvm, na.rm = TRUE)
            }, error = function(e) Inf)
          })
          k_candidates[which.min(devs)]
        }
      }
    )
    
    # Ensure k is within valid bounds
    k <- max(1L, min(k, max_k))
    
    list(pr = pr, k = k)
    
  }, error = function(e) {
    warning(paste("PCR component selection failed:", e$message))
    # Return minimal viable result for graceful degradation
    list(
      pr = list(x = X, center = rep(0, ncol(X)), scale = rep(1, ncol(X))), 
      k = 1L
    )
  })
}

#' Fit PCR with MLE on selected components
#' 
#' @param X Predictor matrix
#' @param y Binary outcome vector
#' @param rule Component selection rule
#' @param foldid CV fold assignments
#' @return List with fitted model, PCA object, and number of components
fit_pcr_mle <- function(X, y, rule, foldid = NULL) {
  # Select components using specified rule
  sel <- select_pcr_components(X, y, rule, foldid)
  
  tryCatch({
    # Extract selected principal components
    Z <- sel$pr$x[, seq_len(sel$k), drop = FALSE]
    
    # Fit logistic regression on components
    fit <- glm(y ~ ., family = binomial(), data = data.frame(y, Z))
    
    list(fit = fit, pr = sel$pr, k = sel$k)
  }, error = function(e) {
    warning(paste("PCR MLE fitting failed:", e$message))
    list(fit = NULL, pr = sel$pr, k = sel$k)
  })
}

#' Predict from PCR model
#' 
#' @param obj PCR model object (from fit_pcr_mle)
#' @param Xnew New predictor matrix
#' @return Vector of predicted probabilities
predict_pcr_mle <- function(obj, Xnew) {
  if (is.null(obj$fit)) {
    # Default prediction when model failed
    return(rep(0.5, nrow(Xnew)))
  }
  
  tryCatch({
    # Transform new data to principal component space
    if (is.list(obj$pr) && is.matrix(obj$pr$x)) {
      # Fallback case - use original data structure (for error recovery)
      Z <- Xnew[, seq_len(obj$k), drop = FALSE]
    } else {
      # Normal case - apply PCA transformation
      Z <- Xnew %*% obj$pr$rotation  # Transform to PC space
      Z <- Z[, seq_len(obj$k), drop = FALSE]  # Keep only selected components
    }
    
    # Predict using logistic regression on components
    as.numeric(predict(obj$fit, newdata = data.frame(Z), type = "response"))
  }, error = function(e) {
    warning(paste("PCR prediction failed:", e$message))
    rep(0.5, nrow(Xnew))  # Default prediction
  })
}

# =======================================================================
# PARTIAL LEAST SQUARES (PLS) FUNCTIONS
# =======================================================================

#' Fit PLS regression with sparse stopping
#' 
#' Uses plsRglm package to fit PLS for binary outcomes with automatic
#' component selection via sparse stopping rule.
#' 
#' @param X Predictor matrix
#' @param y Binary outcome vector
#' @param maxcomp Maximum number of components (default: 30)
#' @param sparseStop Use sparse stopping rule (default: TRUE)
#' @return plsRglm object or NULL if failed
fit_pls <- function(X, y, maxcomp = 30, sparseStop = TRUE) {
  tryCatch({
    # Check for sufficient outcome variation
    if (length(unique(y)) < 2) {
      stop("No variation in outcome - cannot fit PLS model")
    }
    
    # Create data frame for plsRglm
    df <- data.frame(y = y, X)
    
    # Fit PLS regression for binary outcomes
    plsRglm::plsRglm(
      y ~ .,                    # Formula: outcome ~ all predictors
      data = df, 
      nt = min(maxcomp, ncol(X), nrow(X) - 2),  # Max components with df constraint
      modele = "pls-glm-family", # PLS for GLM family
      family = binomial(),       # Logistic regression
      sparse = sparseStop        # Use sparse stopping rule
    )
  }, error = function(e) {
    warning(paste("PLS fitting failed:", e$message))
    NULL
  })
}

#' Predict from PLS model
#' 
#' @param fit plsRglm object
#' @param Xnew New predictor matrix
#' @return Vector of predicted probabilities
predict_pls_resp <- function(fit, Xnew) {
  if (is.null(fit)) {
    # Default prediction when model failed
    return(rep(0.5, nrow(Xnew)))
  }
  
  tryCatch({
    # Convert to data frame with matching structure
    newdata <- data.frame(Xnew)
    
    # Predict using plsRglm
    as.numeric(plsRglm::predict.plsRglm(fit, newdata = newdata, type = "response"))
  }, error = function(e) {
    warning(paste("PLS prediction failed:", e$message))
    rep(0.5, nrow(Xnew))  # Default prediction
  })
}

# =======================================================================
# MAIN EVALUATION FUNCTION
# =======================================================================

#' Evaluate all methods on single dataset
#' 
#' Fits all 11 methods from Lohmann et al. (2023) and computes performance
#' metrics with consistent cross-validation and robust error handling.
#' 
#' @param Xtr Training predictor matrix (standardized)
#' @param ytr Training binary outcomes
#' @param Xva Validation predictor matrix (standardized)
#' @param yva Validation binary outcomes  
#' @param piv True validation probabilities (for rMSPE/MAPE)
#' @param seed_fold Seed for CV fold generation
#' @return Tibble with results for all methods
#' 
#' @details
#' Methods evaluated:
#' 1. MLE (unpenalized logistic regression)
#' 2. Ridge regression (α=0)
#' 3. LASSO (α=1) 
#' 4. Elastic Net (best α from grid)
#' 5. Relaxed LASSO
#' 6-9. PCR with 4 component selection rules
#' 10. PLS with sparse stopping
#' 11. PLS+LASSO hybrid (approximated)
#' 
#' Substitution rules:
#' - Failed specialized methods fall back to MLE
#' - PCR with <2 components falls back to MLE
#' - All methods get consistent CV folds
eval_methods_once <- function(Xtr, ytr, Xva, yva, piv, seed_fold) {
  # Generate consistent CV folds for all methods
  # Use separated seed to avoid interference with data generation
  foldid <- make_folds(nrow(Xtr), K = 10, seed = seed_fold * 17 + 42)
  out <- list()
  
  # Check for degenerate training data
  if (length(unique(ytr)) < 2) {
    warning("No variation in training outcome - returning NA results")
    # Return NA results for all methods
    dummy_metrics <- tibble(
      auc = NA_real_, cal_slope = NA_real_, cal_in_large = NA_real_,
      brier = NA_real_, rMSPE = NA_real_, MAPE = NA_real_
    )
    method_names <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO",
                     "PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", 
                     "PLS", "PLS_LASSO")
    for (method in method_names) {
      out[[method]] <- dummy_metrics
    }
    return(bind_rows(out, .id = "method"))
  }

  # =================================================================
  # 1. MLE (Maximum Likelihood Estimation - unpenalized)
  # =================================================================
  cat("  Fitting MLE...")
  fit_m <- fit_mle(Xtr, ytr)
  ph_m  <- if (is.null(fit_m)) {
    rep(mean(ytr), nrow(Xva))  # Use prevalence as default
  } else {
    predict_mle(fit_m, Xva)
  }
  out$MLE <- compute_metrics(ph_m, yva, piv)
  cat(" done\n")

  # =================================================================
  # 2. Ridge Regression (α = 0, L2 penalty only)
  # =================================================================
  cat("  Fitting Ridge...")
  cv_r <- fit_glmnet_cv(Xtr, ytr, alpha = 0, foldid = foldid)
  ph_r <- if (is.null(cv_r)) {
    ph_m  # Fall back to MLE predictions
  } else {
    predict_glmnet_resp(cv_r, Xva)
  }
  out$Ridge <- compute_metrics(ph_r, yva, piv)
  cat(" done\n")

  # =================================================================
  # 3. LASSO (α = 1, L1 penalty only)
  # =================================================================
  cat("  Fitting LASSO...")
  cv_l <- fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid)
  ph_l <- if (is.null(cv_l)) {
    ph_m  # Fall back to MLE predictions
  } else {
    predict_glmnet_resp(cv_l, Xva)
  }
  out$LASSO <- compute_metrics(ph_l, yva, piv)
  cat(" done\n")

  # =================================================================
  # 4. Elastic Net (scan α grid, pick min CV deviance)
  # =================================================================
  cat("  Fitting Elastic Net...")
  # Test α values as in original paper
  alphas <- c(0, 0.125, 0.25, 0.5, 0.75, 1)
  cv_list <- lapply(alphas, function(a) {
    fit_glmnet_cv(Xtr, ytr, alpha = a, foldid = foldid)
  })
  
  # Find best α based on minimum CV deviance
  if (all(sapply(cv_list, is.null))) {
    out$ElasticNet <- out$MLE  # All fits failed, use MLE
  } else {
    # Compute minimum CV deviance for each α
    devs <- sapply(cv_list, function(cv) {
      if (is.null(cv)) Inf else min(cv$cvm, na.rm = TRUE)
    })
    
    if (all(is.infinite(devs))) {
      out$ElasticNet <- out$MLE  # All deviances infinite, use MLE
    } else {
      best_idx <- which.min(devs)
      ph_en <- predict_glmnet_resp(cv_list[[best_idx]], Xva)
      out$ElasticNet <- compute_metrics(ph_en, yva, piv)
    }
  }
  cat(" done\n")

  # =================================================================
  # 5. Relaxed LASSO (two-stage LASSO procedure)
  # =================================================================
  cat("  Fitting Relaxed LASSO...")
  cv_rl <- fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid, relax = TRUE)
  ph_rl <- if (is.null(cv_rl)) {
    ph_m  # Fall back to MLE predictions
  } else {
    predict_glmnet_resp(cv_rl, Xva)
  }
  out$RelaxedLASSO <- compute_metrics(ph_rl, yva, piv)
  cat(" done\n")

  # =================================================================
  # 6-9. Principal Component Regression (4 selection rules)
  # =================================================================
  pcr_rules <- c("evgt1", "var90", "aic", "cvdev")
  for (rule in pcr_rules) {
    cat(paste0("  Fitting PCR_", rule, "..."))
    
    fitp <- fit_pcr_mle(Xtr, ytr, rule, foldid)
    
    # Apply substitution rule: <2 components -> use MLE
    if (is.null(fitp$fit) || fitp$k < 2) {
      out[[paste0("PCR_", rule)]] <- out$MLE
    } else {
      php <- predict_pcr_mle(fitp, Xva)
      out[[paste0("PCR_", rule)]] <- compute_metrics(php, yva, piv)
    }
    cat(" done\n")
  }

  # =================================================================
  # 10. Partial Least Squares (with sparse stopping)
  # =================================================================
  cat("  Fitting PLS...")
  fit_pl <- fit_pls(Xtr, ytr, maxcomp = 30, sparseStop = TRUE)
  if (is.null(fit_pl)) {
    out$PLS <- out$MLE  # Fall back to MLE
  } else {
    php <- predict_pls_resp(fit_pl, Xva)
    out$PLS <- compute_metrics(php, yva, piv)
  }
  cat(" done\n")

  # =================================================================
  # 11. PLS + LASSO Hybrid (approximated with PCR_cvdev + LASSO)
  # =================================================================
  cat("  Fitting PLS_LASSO...")
  # Use PCR with CV deviance rule as approximation to PLS
  sel <- select_pcr_components(Xtr, ytr, rule = "cvdev", foldid = foldid)
  
  # Apply substitution rule: <2 components -> use MLE
  if (sel$k < 2) {
    out$PLS_LASSO <- out$MLE
  } else {
    # Apply LASSO to selected components
    Ztr <- sel$pr$x[, seq_len(sel$k), drop = FALSE]  # Training components
    cvZ <- fit_glmnet_cv(Ztr, ytr, alpha = 1, foldid = foldid)  # LASSO on components
    
    if (is.null(cvZ)) {
      out$PLS_LASSO <- out$MLE  # LASSO failed, use MLE
    } else {
      # Transform validation data to component space and predict
      if (is.list(sel$pr) && is.matrix(sel$pr$x)) {
        # Fallback for error recovery
        Zva <- Xva[, seq_len(sel$k), drop = FALSE]
      } else {
        # Normal transformation
        Zva <- Xva %*% sel$pr$rotation
        Zva <- Zva[, seq_len(sel$k), drop = FALSE]
      }
      
      php <- predict_glmnet_resp(cvZ, Zva)
      out$PLS_LASSO <- compute_metrics(php, yva, piv)
    }
  }
  cat(" done\n")

  # Return results as tibble with method names
  bind_rows(out, .id = "method")
}

# =======================================================================
# RANKING AND AGGREGATION FUNCTIONS
# =======================================================================

#' Apply Lohmann et al. (2023) ranking rules
#' 
#' Implements exact rounding and ranking methodology from the original paper
#' 
#' @param df Data frame with performance metrics
#' @return Data frame with added ranking columns
#' 
#' @details
#' Ranking rules:
#' - AUC: 3 decimals, higher better, minimum rank for ties
#' - Brier: 3 decimals, lower better, minimum rank for ties  
#' - Cal slope: 2 decimals, rank by |slope - 1|, minimum rank for ties
#' - rMSPE/MAPE: 3 decimals, lower better, minimum rank for ties
rank_with_rounding <- function(df) {
  df %>%
    mutate(
      # AUC: higher is better, round to 3 decimals
      r_auc   = rank(-round(auc, 3), ties.method = "min", na.last = "keep"),
      
      # Brier score: lower is better, round to 3 decimals  
      r_brier = rank(round(brier, 3), ties.method = "min", na.last = "keep"),
      
      # Calibration slope: closer to 1 is better, round to 2 decimals
      r_cals  = rank(abs(round(cal_slope, 2) - 1), ties.method = "min", na.last = "keep"),
      
      # rMSPE: lower is better, round to 3 decimals
      r_rmspe = rank(round(rMSPE, 3), ties.method = "min", na.last = "keep"),
      
      # MAPE: lower is better, round to 3 decimals
      r_mape  = rank(round(MAPE, 3), ties.method = "min", na.last = "keep")
    )
}

# =======================================================================
# SIMULATION SCENARIO MANAGEMENT
# =======================================================================

#' Generate complete simulation grid
#' 
#' Creates the exact 1,050 scenario grid from Lohmann et al. (2023)
#' 
#' @return Tibble with all scenario combinations and unique identifiers
#' 
#' @details
#' Grid dimensions:
#' - EPV: 7 levels (3, 5, 10, 15, 20, 50, 100)
#' - Event fraction: 5 levels (1/32, 1/16, 1/8, 1/4, 1/2)  
#' - Predictors: 5 levels (4, 8, 16, 32, 64)
#' - Noise fraction: 3 levels (0, 1/4, 1/2)
#' - Sparse predictors: 2 levels (FALSE, TRUE)
#' Total: 7 × 5 × 5 × 3 × 2 = 1,050 scenarios
make_full_grid <- function() {
  grid <- expand.grid(
    EPV         = c(3, 5, 10, 15, 20, 50, 100),        # Events per variable
    event_frac  = c(1/32, 1/16, 1/8, 1/4, 1/2),       # Event probability  
    p           = c(4, 8, 16, 32, 64),                 # Number of predictors
    noise_frac  = c(0, 1/4, 1/2),                     # Fraction of noise predictors
    sparse      = c(FALSE, TRUE),                      # Binary predictors flag
    KEEP.OUT.ATTRS = FALSE,                            # Clean output
    stringsAsFactors = FALSE                           # Avoid factors
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(scn_id = dplyr::row_number())        # Add unique scenario ID
  
  # Validate grid size
  if (nrow(grid) != 1050) {
    stop(paste("Expected 1050 scenarios, got", nrow(grid)))
  }
  
  grid
}

#' Run simulation for a block of scenarios
#' 
#' Main simulation function that processes a subset of scenarios with
#' multiple iterations and comprehensive error handling.
#' 
#' @param grid Complete scenario grid
#' @param scn_indices Vector of scenario indices to process
#' @param iters Number of iterations per scenario (default: 20)
#' @param base_seed Base random seed for reproducibility
#' @return Tibble with results for all scenario × iteration × method combinations
#' 
#' @details
#' For each scenario:
#' 1. Generate `iters` independent datasets
#' 2. Fit all 11 methods on each dataset
#' 3. Compute 6 performance metrics
#' 4. Handle errors gracefully with informative warnings
#' 5. Return structured results with scenario metadata
run_scenarios_block <- function(grid, scn_indices, iters = 20,
                                base_seed = 20250923L) {
  
  # Initialize result storage
  out <- vector("list", length(scn_indices))
  
  # Process each scenario
  for (i in seq_along(scn_indices)) {
    scn_idx <- scn_indices[i]
    s <- grid[scn_idx, ]  # Current scenario parameters
    
    # Log progress
    cat(sprintf("Running scenario %d/%d (ID: %d) - EPV: %d, p: %d, event_frac: %.4f, noise: %.2f, sparse: %s\n",
                i, length(scn_indices), s$scn_id, s$EPV, s$p, 
                s$event_frac, s$noise_frac, s$sparse))
    
    # Run multiple iterations for this scenario
    res_iter <- vector("list", iters)
    for (it in seq_len(iters)) {
      # Generate unique seed for this scenario × iteration
      seed <- base_seed + s$scn_id * 1000L + it
      
      cat(sprintf("  Iteration %d/%d (seed: %d)\n", it, iters, seed))
      
      tryCatch({
        # Generate dataset for this iteration
        dat <- gen_dataset(
          p = s$p, 
          event_frac = s$event_frac, 
          EPV = s$EPV,
          noise_frac = s$noise_frac, 
          sparse = s$sparse, 
          seed = seed
        )
        
        # Evaluate all methods on this dataset
        met <- eval_methods_once(
          Xtr = dat$Xtr, ytr = dat$ytr, 
          Xva = dat$Xva, yva = dat$yva, 
          piv = dat$piv,
          seed_fold = seed + 13L  # Offset seed for CV folds
        ) %>%
          mutate(iter = it)  # Add iteration identifier
        
        res_iter[[it]] <- met
        
      }, error = function(e) {
        # Log error and return dummy results
        warning(sprintf("Scenario %d iteration %d failed: %s", 
                       s$scn_id, it, e$message))
        
        # Create dummy results with NAs
        dummy_metrics <- tibble(
          auc = NA_real_, cal_slope = NA_real_, cal_in_large = NA_real_,
          brier = NA_real_, rMSPE = NA_real_, MAPE = NA_real_
        )
        method_names <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO",
                         "PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", 
                         "PLS", "PLS_LASSO")
        
        dummy_df <- bind_rows(
          lapply(method_names, function(m) dummy_metrics), 
          .id = "method"
        ) %>%
          mutate(method = method_names, iter = it)
        
        res_iter[[it]] <- dummy_df
      })
    }
    
    # Combine iterations for this scenario and add metadata
    scenario_results <- bind_rows(res_iter) %>%
      mutate(
        scn_id = s$scn_id,
        EPV = s$EPV, 
        event_frac = s$event_frac, 
        p = s$p,
        noise_frac = s$noise_frac, 
        sparse = s$sparse
      )
    
    out[[i]] <- scenario_results
    
    # Clean up memory periodically
    if (i %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  # Combine all scenarios and return
  final_results <- bind_rows(out)
  
  cat(sprintf("\nCompleted %d scenarios, %d total evaluations\n", 
              length(scn_indices), nrow(final_results)))
  
  final_results
}

# =======================================================================
# PACKAGE LOADING VERIFICATION
# =======================================================================

# Verify all required packages are available at load time
cat("Lohmann et al. (2023) replication framework loaded successfully.\n")
cat("Required packages verified: MASS, glmnet, pROC, plsRglm, Matrix, dplyr, purrr, tibble, tidyr\n")
cat("\nMain functions available:\n")
cat("- make_full_grid(): Generate 1,050 scenario combinations\n")
cat("- gen_dataset(): Generate data for one scenario\n") 
cat("- eval_methods_once(): Evaluate all 11 methods on one dataset\n")
cat("- run_scenarios_block(): Run simulation block with multiple scenarios\n")
cat("- rank_with_rounding(): Apply Lohmann et al. ranking rules\n")
cat("\nFor usage examples, see README.md\n")
