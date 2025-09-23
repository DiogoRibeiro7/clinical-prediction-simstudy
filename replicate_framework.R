# =======================================================================
# replicate_framework.R
# Core logic for the Lohmann et al. (2023) simulation replication
# FIXED VERSION - addresses data standardization and robustness issues
# =======================================================================

suppressPackageStartupMessages({
  library(MASS)
  library(glmnet)
  library(pROC)
  library(plsRglm)
  library(Matrix)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

# ---------- Numerics ----------------------------------------------------
clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)
logit <- function(p, eps = 1e-12) { p <- clip(p, eps, 1 - eps); log(p/(1-p)) }

winsorize_cal_slope <- function(b, lo = 0.01, hi = 10) clip(b, lo, hi)

# ---------- Correlation / data gen --------------------------------------
make_sigma <- function(p, beta_a = 1, beta_b = 3) {
  r_abs <- rbeta(n = p * (p - 1) / 2, shape1 = beta_a, shape2 = beta_b)
  signs <- sample(c(-1, 1), length(r_abs), replace = TRUE)
  r <- r_abs * signs
  Sigma <- diag(1, p)
  k <- 1L
  for (i in seq_len(p - 1L)) {
    for (j in (i + 1L):p) {
      Sigma[i, j] <- r[k]; Sigma[j, i] <- r[k]; k <- k + 1L
    }
  }
  eig <- eigen(Sigma, symmetric = TRUE)
  # More robust eigenvalue fixing
  min_eig <- max(1e-8, min(eig$values) * 0.001)
  eig$values[eig$values < min_eig] <- min_eig
  S <- eig$vectors %*% diag(eig$values, p) %*% t(eig$vectors)
  D <- diag(1 / sqrt(diag(S)), p)
  as.matrix(D %*% S %*% D)
}

solve_intercept <- function(beta, X, target_pi, tol = 1e-10, max_iter = 100) {
  # More robust intercept solving with better bounds and iteration limit
  f <- function(b0) mean(plogis(b0 + as.numeric(X %*% beta))) - target_pi
  
  # Try wider bounds first
  tryCatch({
    uniroot(f, c(-20, 20), tol = tol, maxiter = max_iter)$root
  }, error = function(e1) {
    # If that fails, try even wider bounds
    tryCatch({
      uniroot(f, c(-50, 50), tol = tol * 10, maxiter = max_iter)$root
    }, error = function(e2) {
      # If still fails, use a simple approximation
      warning("Intercept solving failed, using approximation")
      qlogis(target_pi)
    })
  })
}

draw_true_beta <- function(p, noise_frac = 0) {
  beta <- rnorm(p, 0, 1)
  if (noise_frac > 0) {
    k0 <- floor(noise_frac * p)
    if (k0 > 0) beta[sample(seq_len(p), k0)] <- 0
  }
  beta
}

gen_dataset <- function(p, event_frac, EPV, noise_frac, sparse, seed,
                        n_val_scale = 20) {
  set.seed(seed)
  # EPV = expected_events / p => n_train = (EPV * p) / event_frac
  n_train <- max(200L, ceiling((EPV * p) / event_frac))
  n_val   <- max(200L, ceiling(n_val_scale * p / event_frac))

  Sigma <- make_sigma(p)
  Xtr <- mvrnorm(n_train, rep(0, p), Sigma)
  Xva <- mvrnorm(n_val,   rep(0, p), Sigma)

  # FIXED: Standardize FIRST, then apply sparse transformation
  Xtr_s <- scale(Xtr, center = TRUE, scale = TRUE)
  scale_center <- attr(Xtr_s, "scaled:center")
  scale_scale <- attr(Xtr_s, "scaled:scale")
  Xva_s <- scale(Xva, center = scale_center, scale = scale_scale)

  # Now apply sparse predictors to standardized data
  if (isTRUE(sparse)) {
    idx <- sample(seq_len(p), max(1L, floor(p/4)))  # Ensure at least 1 sparse predictor
    # Convert to binary based on standardized values
    Xtr_s[, idx] <- (Xtr_s[, idx] > 0) * 1
    Xva_s[, idx] <- (Xva_s[, idx] > 0) * 1
  }

  beta_true <- draw_true_beta(p, noise_frac)
  b0_true   <- solve_intercept(beta_true, Xtr_s, target_pi = event_frac)

  pi_tr <- plogis(b0_true + as.numeric(Xtr_s %*% beta_true))
  pi_va <- plogis(b0_true + as.numeric(Xva_s %*% beta_true))

  y_tr <- rbinom(n_train, 1, pi_tr)
  y_va <- rbinom(n_val,   1, pi_va)

  list(
    Xtr = Xtr_s, ytr = y_tr, piv = pi_va, Xva = Xva_s, yva = y_va,
    beta_true = beta_true, b0_true = b0_true,
    scale_center = scale_center, scale_scale = scale_scale  # For debugging
  )
}

# ---------- Metrics ----------------------------------------------------------
compute_metrics <- function(phat, y, pi_true) {
  # More robust AUC computation
  auc <- tryCatch({
    if (length(unique(y)) < 2 || length(unique(phat)) < 2) {
      NA_real_
    } else {
      as.numeric(pROC::auc(y, phat, quiet = TRUE))
    }
  }, error = function(e) NA_real_)
  
  # More robust calibration slope computation
  lph <- logit(phat)
  cal_fit <- tryCatch({
    if (any(is.infinite(lph)) || any(is.na(lph)) || length(unique(y)) < 2) {
      NULL
    } else {
      glm(y ~ lph, family = binomial())
    }
  }, error = function(e) NULL)
  
  b <- if (is.null(cal_fit) || any(is.na(coef(cal_fit)))) {
    NA_real_
  } else {
    winsorize_cal_slope(unname(coef(cal_fit)[2]))
  }
  
  # Calibration-in-the-large
  cil_fit <- tryCatch({
    if (any(is.infinite(lph)) || any(is.na(lph)) || length(unique(y)) < 2) {
      NULL
    } else {
      glm(y ~ 1, family = binomial(), offset = lph)
    }
  }, error = function(e) NULL)
  
  cil <- if (is.null(cil_fit) || any(is.na(coef(cil_fit)))) {
    NA_real_
  } else {
    unname(coef(cil_fit)[1])
  }

  brier <- mean((y - phat)^2)
  rMSPE <- sqrt(mean((pi_true - phat)^2))
  MAPE  <- mean(abs(pi_true - phat))

  tibble(auc = auc, cal_slope = b, cal_in_large = cil,
         brier = brier, rMSPE = rMSPE, MAPE = MAPE)
}

# ---------- Shared CV folds ---------------------------------------------------
make_folds <- function(n, K = 10, seed = 1) {
  # More robust fold creation
  set.seed(seed)
  if (n < K) {
    # If fewer observations than folds, each obs gets its own fold
    seq_len(n)
  } else {
    sample(rep(1:K, length.out = n))
  }
}

# ---------- Models ------------------------------------------------------------
fit_mle <- function(X, y) {
  # More robust MLE fitting
  tryCatch({
    df <- as.data.frame(X)
    colnames(df) <- paste0("x", seq_len(ncol(df)))
    df$y <- y
    
    # Check for perfect separation
    if (length(unique(y)) < 2) {
      stop("No variation in outcome")
    }
    
    fit <- glm(y ~ ., data = df, family = binomial())
    
    # Check for convergence issues
    if (!fit$converged) {
      warning("MLE did not converge")
    }
    
    fit
  }, error = function(e) {
    warning(paste("MLE failed:", e$message))
    NULL
  })
}

predict_mle <- function(fit, X) {
  if (is.null(fit)) {
    return(rep(0.5, nrow(X)))
  }
  
  tryCatch({
    df <- as.data.frame(X)
    colnames(df) <- names(coef(fit))[-1]
    as.numeric(predict(fit, df, type = "response"))
  }, error = function(e) {
    warning(paste("MLE prediction failed:", e$message))
    rep(0.5, nrow(X))
  })
}

fit_glmnet_cv <- function(X, y, alpha, foldid, relax = FALSE) {
  tryCatch({
    # Check for sufficient variation
    if (length(unique(y)) < 2) {
      stop("No variation in outcome")
    }
    
    cv.glmnet(
      x = X, y = y, family = "binomial", alpha = alpha, relax = relax,
      foldid = foldid, nlambda = 100, type.measure = "deviance",
      standardize = FALSE  # We already standardized
    )
  }, error = function(e) {
    warning(paste("glmnet failed:", e$message))
    NULL
  })
}

predict_glmnet_resp <- function(cvfit, X) {
  if (is.null(cvfit)) {
    return(rep(0.5, nrow(X)))
  }
  
  tryCatch({
    as.numeric(predict(cvfit, newx = X, type = "response", s = "lambda.min"))
  }, error = function(e) {
    warning(paste("glmnet prediction failed:", e$message))
    rep(0.5, nrow(X))
  })
}

# ----- PCR component selection rules -----------------------------------------
select_pcr_components <- function(X, y, rule = c("evgt1","var90","aic","cvdev"), foldid = NULL) {
  rule <- match.arg(rule)
  
  tryCatch({
    pr <- prcomp(X, center = FALSE, scale. = FALSE)  # Already standardized
    eig <- pr$sdev^2
    varexpl <- cumsum(eig) / sum(eig)
    k_candidates <- seq_len(min(ncol(X), nrow(X) - 2))  # Ensure df for GLM

    k <- switch(
      rule,
      evgt1 = max(1L, sum(eig > 1)),
      var90 = max(1L, which(varexpl >= 0.90)[1]),
      aic   = {
        if (length(unique(y)) < 2) {
          1L
        } else {
          aics <- sapply(k_candidates, function(k) {
            tryCatch({
              Z <- pr$x[, seq_len(k), drop = FALSE]
              fit <- glm(y ~ ., family = binomial(), data = data.frame(y, Z))
              if (fit$converged) AIC(fit) else Inf
            }, error = function(e) Inf)
          })
          k_candidates[which.min(aics)]
        }
      },
      cvdev = {
        if (is.null(foldid) || length(unique(y)) < 2) {
          1L
        } else {
          devs <- sapply(k_candidates, function(k) {
            tryCatch({
              Z <- pr$x[, seq_len(k), drop = FALSE]
              cv <- cv.glmnet(Z, y, family = "binomial", alpha = 0,
                              foldid = foldid, type.measure = "deviance",
                              standardize = FALSE)
              min(cv$cvm)
            }, error = function(e) Inf)
          })
          k_candidates[which.min(devs)]
        }
      }
    )
    k <- max(1L, min(k, ncol(X), nrow(X) - 2))
    list(pr = pr, k = k)
  }, error = function(e) {
    warning(paste("PCR component selection failed:", e$message))
    # Return minimal viable result
    list(pr = list(x = X, center = rep(0, ncol(X)), scale = rep(1, ncol(X))), k = 1L)
  })
}

fit_pcr_mle <- function(X, y, rule, foldid = NULL) {
  sel <- select_pcr_components(X, y, rule, foldid)
  
  tryCatch({
    Z <- sel$pr$x[, seq_len(sel$k), drop = FALSE]
    fit <- glm(y ~ ., family = binomial(), data = data.frame(y, Z))
    list(fit = fit, pr = sel$pr, k = sel$k)
  }, error = function(e) {
    warning(paste("PCR MLE failed:", e$message))
    list(fit = NULL, pr = sel$pr, k = sel$k)
  })
}

predict_pcr_mle <- function(obj, Xnew) {
  if (is.null(obj$fit)) {
    return(rep(0.5, nrow(Xnew)))
  }
  
  tryCatch({
    # For our case, Xnew is already standardized, so we just need the rotation
    if (is.list(obj$pr) && is.matrix(obj$pr$x)) {
      # Fallback case - use original data structure
      Z <- Xnew[, seq_len(obj$k), drop = FALSE]
    } else {
      # Normal case - apply PCA rotation
      Z <- Xnew %*% obj$pr$rotation
      Z <- Z[, seq_len(obj$k), drop = FALSE]
    }
    as.numeric(predict(obj$fit, newdata = data.frame(Z), type = "response"))
  }, error = function(e) {
    warning(paste("PCR prediction failed:", e$message))
    rep(0.5, nrow(Xnew))
  })
}

# ----- PLS (with stopping) ----------------------------------------------------
fit_pls <- function(X, y, maxcomp = 30, sparseStop = TRUE) {
  tryCatch({
    if (length(unique(y)) < 2) {
      stop("No variation in outcome")
    }
    
    df <- data.frame(y = y, X)
    plsRglm::plsRglm(
      y ~ ., data = df, nt = min(maxcomp, ncol(X), nrow(X) - 2),
      modele = "pls-glm-family", family = binomial(), sparse = sparseStop
    )
  }, error = function(e) {
    warning(paste("PLS failed:", e$message))
    NULL
  })
}

predict_pls_resp <- function(fit, Xnew) {
  if (is.null(fit)) {
    return(rep(0.5, nrow(Xnew)))
  }
  
  tryCatch({
    as.numeric(plsRglm::predict.plsRglm(fit, newdata = data.frame(Xnew), type = "response"))
  }, error = function(e) {
    warning(paste("PLS prediction failed:", e$message))
    rep(0.5, nrow(Xnew))
  })
}

# ----- One iteration (all methods) with substitution rules --------------------
eval_methods_once <- function(Xtr, ytr, Xva, yva, piv, seed_fold) {
  # Use a more separated seed for fold generation
  foldid <- make_folds(nrow(Xtr), K = 10, seed = seed_fold * 17 + 42)
  out <- list()
  
  # Check for degenerate cases
  if (length(unique(ytr)) < 2) {
    warning("No variation in training outcome - returning NA results")
    dummy_metrics <- tibble(auc = NA_real_, cal_slope = NA_real_, 
                           cal_in_large = NA_real_, brier = NA_real_,
                           rMSPE = NA_real_, MAPE = NA_real_)
    method_names <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO",
                     "PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", 
                     "PLS", "PLS_LASSO")
    for (method in method_names) {
      out[[method]] <- dummy_metrics
    }
    return(bind_rows(out, .id = "method"))
  }

  # MLE
  fit_m <- fit_mle(Xtr, ytr)
  ph_m  <- if (is.null(fit_m)) rep(mean(ytr), nrow(Xva)) else predict_mle(fit_m, Xva)
  out$MLE <- compute_metrics(ph_m, yva, piv)

  # Ridge
  cv_r <- fit_glmnet_cv(Xtr, ytr, alpha = 0, foldid = foldid)
  ph_r <- if (is.null(cv_r)) ph_m else predict_glmnet_resp(cv_r, Xva)
  out$Ridge <- compute_metrics(ph_r, yva, piv)

  # LASSO
  cv_l <- fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid)
  ph_l <- if (is.null(cv_l)) ph_m else predict_glmnet_resp(cv_l, Xva)
  out$LASSO <- compute_metrics(ph_l, yva, piv)

  # Elastic Net: scan α grid, pick min CV deviance
  alphas <- c(0, 0.125, 0.25, 0.5, 0.75, 1)
  cv_list <- lapply(alphas, function(a) fit_glmnet_cv(Xtr, ytr, alpha = a, foldid = foldid))
  
  if (all(sapply(cv_list, is.null))) {
    out$ElasticNet <- out$MLE
  } else {
    devs <- sapply(cv_list, function(cv) if (is.null(cv)) Inf else min(cv$cvm, na.rm = TRUE))
    if (all(is.infinite(devs))) {
      out$ElasticNet <- out$MLE
    } else {
      best <- which.min(devs)
      ph_en <- predict_glmnet_resp(cv_list[[best]], Xva)
      out$ElasticNet <- compute_metrics(ph_en, yva, piv)
    }
  }

  # Relaxed LASSO
  cv_rl <- fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid, relax = TRUE)
  ph_rl <- if (is.null(cv_rl)) ph_m else predict_glmnet_resp(cv_rl, Xva)
  out$RelaxedLASSO <- compute_metrics(ph_rl, yva, piv)

  # PCR rules
  for (rule in c("evgt1","var90","aic","cvdev")) {
    fitp <- fit_pcr_mle(Xtr, ytr, rule, foldid)
    if (is.null(fitp$fit) || fitp$k < 2) {
      out[[paste0("PCR_", rule)]] <- out$MLE
    } else {
      php <- predict_pcr_mle(fitp, Xva)
      out[[paste0("PCR_", rule)]] <- compute_metrics(php, yva, piv)
    }
  }

  # PLS (stopping), cap 30 comps
  fit_pl <- fit_pls(Xtr, ytr, maxcomp = 30, sparseStop = TRUE)
  if (is.null(fit_pl)) {
    out$PLS <- out$MLE
  } else {
    php <- predict_pls_resp(fit_pl, Xva)
    out$PLS <- compute_metrics(php, yva, piv)
  }

  # PLS + LASSO hybrid (using PCR_cvdev approximation)
  sel <- select_pcr_components(Xtr, ytr, rule = "cvdev", foldid = foldid)
  if (sel$k < 2) {
    out$PLS_LASSO <- out$MLE
  } else {
    Ztr <- sel$pr$x[, seq_len(sel$k), drop = FALSE]
    cvZ <- fit_glmnet_cv(Ztr, ytr, alpha = 1, foldid = foldid)
    if (is.null(cvZ)) {
      out$PLS_LASSO <- out$MLE
    } else {
      # Apply same transformation to validation set
      if (is.list(sel$pr) && is.matrix(sel$pr$x)) {
        Zva <- Xva[, seq_len(sel$k), drop = FALSE]
      } else {
        Zva <- Xva %*% sel$pr$rotation
        Zva <- Zva[, seq_len(sel$k), drop = FALSE]
      }
      php <- predict_glmnet_resp(cvZ, Zva)
      out$PLS_LASSO <- compute_metrics(php, yva, piv)
    }
  }

  bind_rows(out, .id = "method")
}

# ---------- Ranking (paper's rounding rules) ---------------------------------
rank_with_rounding <- function(df) {
  df %>%
    mutate(
      r_auc   = rank(-round(auc,   3), ties.method = "min", na.last = "keep"),
      r_brier = rank( round(brier, 3), ties.method = "min", na.last = "keep"),
      r_cals  = rank( abs(round(cal_slope, 2) - 1), ties.method = "min", na.last = "keep"),
      r_rmspe = rank( round(rMSPE, 3), ties.method = "min", na.last = "keep"),
      r_mape  = rank( round(MAPE,  3), ties.method = "min", na.last = "keep")
    )
}

# ---------- Scenario grid (exact 1050) ---------------------------------------
make_full_grid <- function() {
  expand.grid(
    EPV         = c(3,5,10,15,20,50,100),
    event_frac  = c(1/32, 1/16, 1/8, 1/4, 1/2),
    p           = c(4,8,16,32,64),
    noise_frac  = c(0, 1/4, 1/2),
    sparse      = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(scn_id = dplyr::row_number())
}

# ---------- Run a block of scenarios -----------------------------------------
run_scenarios_block <- function(grid, scn_indices, iters = 20,
                                base_seed = 20250923L) {
  out <- vector("list", length(scn_indices))
  for (i in seq_along(scn_indices)) {
    s <- grid[scn_indices[i], ]
    cat("Running scenario", s$scn_id, "- EPV:", s$EPV, "p:", s$p, 
        "event_frac:", round(s$event_frac, 4), "\n")
    
    res_iter <- vector("list", iters)
    for (it in seq_len(iters)) {
      seed <- base_seed + s$scn_id * 1000L + it
      
      tryCatch({
        dat <- gen_dataset(
          p = s$p, event_frac = s$event_frac, EPV = s$EPV,
          noise_frac = s$noise_frac, sparse = s$sparse, seed = seed
        )
        met <- eval_methods_once(dat$Xtr, dat$ytr, dat$Xva, dat$yva, dat$piv,
                                 seed_fold = seed + 13L) %>%
          mutate(iter = it)
        res_iter[[it]] <- met
      }, error = function(e) {
        warning(paste("Scenario", s$scn_id, "iteration", it, "failed:", e$message))
        # Return dummy results with NAs
        dummy_metrics <- tibble(auc = NA_real_, cal_slope = NA_real_, 
                               cal_in_large = NA_real_, brier = NA_real_,
                               rMSPE = NA_real_, MAPE = NA_real_)
        method_names <- c("MLE", "Ridge", "LASSO", "ElasticNet", "RelaxedLASSO",
                         "PCR_evgt1", "PCR_var90", "PCR_aic", "PCR_cvdev", 
                         "PLS", "PLS_LASSO")
        dummy_df <- bind_rows(lapply(method_names, function(m) dummy_metrics), .id = "method") %>%
          mutate(method = method_names, iter = it)
        res_iter[[it]] <- dummy_df
      })
    }
    out[[i]] <- bind_rows(res_iter) %>%
      mutate(scn_id = s$scn_id,
             EPV = s$EPV, event_frac = s$event_frac, p = s$p,
             noise_frac = s$noise_frac, sparse = s$sparse)
  }
  bind_rows(out)
}
