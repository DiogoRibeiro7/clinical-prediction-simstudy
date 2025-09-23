# =======================================================================
# replicate_framework.R
# Core logic for the Lohmann et al. (2023) simulation replication
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
  eig$values[eig$values < 1e-6] <- 1e-6
  S <- eig$vectors %*% diag(eig$values, p) %*% t(eig$vectors)
  D <- diag(1 / sqrt(diag(S)), p)
  as.matrix(D %*% S %*% D)
}

solve_intercept <- function(beta, X, target_pi, tol = 1e-10) {
  f <- function(b0) mean(plogis(b0 + as.numeric(X %*% beta))) - target_pi
  uniroot(f, c(-50, 50), tol = tol)$root
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

  # Optional sparse predictors: make 1/4 binary (supervised choice in paper’s spirit)
  if (isTRUE(sparse)) {
    idx <- sample(seq_len(p), floor(p/4))
    Xtr[, idx] <- (Xtr[, idx] > 0) * 1
    Xva[, idx] <- (Xva[, idx] > 0) * 1
  }

  # Standardize (like paper’s centering assumption)
  Xtr_s <- scale(Xtr, center = TRUE, scale = TRUE)
  Xva_s <- scale(Xva, center = attr(Xtr_s, "scaled:center"),
                      scale  = attr(Xtr_s, "scaled:scale"))

  beta_true <- draw_true_beta(p, noise_frac)
  b0_true   <- solve_intercept(beta_true, Xtr_s, target_pi = event_frac)

  pi_tr <- plogis(b0_true + as.numeric(Xtr_s %*% beta_true))
  pi_va <- plogis(b0_true + as.numeric(Xva_s %*% beta_true))

  y_tr <- rbinom(n_train, 1, pi_tr)
  y_va <- rbinom(n_val,   1, pi_va)

  list(
    Xtr = Xtr_s, ytr = y_tr, piv = pi_va, Xva = Xva_s, yva = y_va,
    beta_true = beta_true, b0_true = b0_true
  )
}

# ---------- Metrics ----------------------------------------------------------
compute_metrics <- function(phat, y, pi_true) {
  auc <- tryCatch(as.numeric(pROC::auc(y, phat)), error = function(e) NA_real_)
  # Recalibration: logit(p) = a + b*logit(phat)
  lph <- logit(phat)
  cal_fit <- tryCatch(glm(y ~ lph, family = binomial()), error = function(e) NULL)
  b <- if (is.null(cal_fit)) NA_real_ else unname(coef(cal_fit)[2])
  b <- if (is.na(b)) NA_real_ else winsorize_cal_slope(b)
  cil_fit <- tryCatch(glm(y ~ 1, family = binomial(), offset = lph), error = function(e) NULL)
  cil <- if (is.null(cil_fit)) NA_real_ else unname(coef(cil_fit)[1])

  brier <- mean((y - phat)^2)
  rMSPE <- sqrt(mean((pi_true - phat)^2))
  MAPE  <- mean(abs(pi_true - phat))

  tibble(auc = auc, cal_slope = b, cal_in_large = cil,
         brier = brier, rMSPE = rMSPE, MAPE = MAPE)
}

# ---------- Shared CV folds ---------------------------------------------------
make_folds <- function(n, K = 10, seed = 1) {
  set.seed(seed); sample(rep(1:K, length.out = n))
}

# ---------- Models ------------------------------------------------------------
fit_mle <- function(X, y) {
  df <- as.data.frame(X); colnames(df) <- paste0("x", seq_len(ncol(df))); df$y <- y
  glm(y ~ ., data = df, family = binomial())
}
predict_mle <- function(fit, X) {
  df <- as.data.frame(X); colnames(df) <- names(coef(fit))[-1]
  as.numeric(predict(fit, df, type = "response"))
}

fit_glmnet_cv <- function(X, y, alpha, foldid, relax = FALSE) {
  cv.glmnet(
    x = X, y = y, family = "binomial", alpha = alpha, relax = relax,
    foldid = foldid, nlambda = 100, type.measure = "deviance"
  )
}
predict_glmnet_resp <- function(cvfit, X) {
  as.numeric(predict(cvfit, newx = X, type = "response", s = "lambda.min"))
}

# ----- PCR component selection rules -----------------------------------------
select_pcr_components <- function(X, y, rule = c("evgt1","var90","aic","cvdev"), foldid = NULL) {
  rule <- match.arg(rule)
  pr <- prcomp(X, center = FALSE, scale. = FALSE)
  eig <- pr$sdev^2
  varexpl <- cumsum(eig) / sum(eig)
  k_candidates <- seq_len(ncol(X))

  k <- switch(
    rule,
    evgt1 = sum(eig > 1),
    var90 = which(varexpl >= 0.90)[1],
    aic   = {
      aics <- sapply(k_candidates, function(k) {
        Z <- pr$x[, seq_len(k), drop = FALSE]
        fit <- glm(y ~ ., family = binomial(), data = data.frame(y, Z))
        AIC(fit)
      })
      k_candidates[which.min(aics)]
    },
    cvdev = {
      if (is.null(foldid)) stop("foldid required for cvdev")
      devs <- sapply(k_candidates, function(k) {
        Z <- pr$x[, seq_len(k), drop = FALSE]
        cv <- cv.glmnet(Z, y, family = "binomial", alpha = 0, # ridge on PCs
                        foldid = foldid, type.measure = "deviance")
        min(cv$cvm)
      })
      k_candidates[which.min(devs)]
    }
  )
  k <- max(1L, min(k, ncol(X)))
  list(pr = pr, k = k)
}

fit_pcr_mle <- function(X, y, rule, foldid = NULL) {
  sel <- select_pcr_components(X, y, rule, foldid)
  list(
    fit = glm(y ~ ., family = binomial(), data = data.frame(y, sel$pr$x[, seq_len(sel$k), drop = FALSE])),
    pr  = sel$pr,
    k   = sel$k
  )
}
predict_pcr_mle <- function(obj, Xnew) {
  Z <- scale(Xnew, center = obj$pr$center, scale = obj$pr$scale) %*% obj$pr$rotation
  Z <- Z[, seq_len(obj$k), drop = FALSE]
  as.numeric(predict(obj$fit, newdata = data.frame(Z), type = "response"))
}

# ----- PLS (with stopping) ----------------------------------------------------
fit_pls <- function(X, y, maxcomp = 30, sparseStop = TRUE) {
  df <- data.frame(y = y, X)
  plsRglm::plsRglm(
    y ~ ., data = df, nt = min(maxcomp, ncol(X)),
    modele = "pls-glm-family", family = binomial(), sparse = sparseStop
  )
}
predict_pls_resp <- function(fit, Xnew) {
  as.numeric(plsRglm::predict.plsRglm(fit, newdata = data.frame(Xnew), type = "response"))
}

# ----- One iteration (all methods) with substitution rules --------------------
eval_methods_once <- function(Xtr, ytr, Xva, yva, piv, seed_fold) {
  foldid <- make_folds(nrow(Xtr), K = 10, seed = seed_fold)
  out <- list()

  # MLE
  fit_m <- tryCatch(fit_mle(Xtr, ytr), error = function(e) NULL)
  ph_m  <- if (is.null(fit_m)) rep(mean(ytr), nrow(Xva)) else predict_mle(fit_m, Xva)
  out$MLE <- compute_metrics(ph_m, yva, piv)

  # Ridge
  cv_r <- tryCatch(fit_glmnet_cv(Xtr, ytr, alpha = 0, foldid = foldid), error = function(e) NULL)
  ph_r <- if (is.null(cv_r)) ph_m else predict_glmnet_resp(cv_r, Xva)
  out$Ridge <- compute_metrics(ph_r, yva, piv)

  # LASSO
  cv_l <- tryCatch(fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid), error = function(e) NULL)
  ph_l <- if (is.null(cv_l)) ph_m else predict_glmnet_resp(cv_l, Xva)
  # If LASSO selected no predictors, glmnet still returns intercept-only; this is fine.
  out$LASSO <- compute_metrics(ph_l, yva, piv)

  # Elastic Net: scan α grid, pick min CV deviance
  alphas <- c(0, 0.125, 0.25, 0.5, 0.75, 1)
  cv_list <- lapply(alphas, function(a) tryCatch(
    fit_glmnet_cv(Xtr, ytr, alpha = a, foldid = foldid),
    error = function(e) NULL))
  if (all(sapply(cv_list, is.null))) {
    out$ElasticNet <- out$MLE
  } else {
    devs <- sapply(cv_list, function(cv) if (is.null(cv)) Inf else min(cv$cvm))
    best <- which.min(devs)
    ph_en <- predict_glmnet_resp(cv_list[[best]], Xva)
    out$ElasticNet <- compute_metrics(ph_en, yva, piv)
  }

  # Relaxed LASSO
  cv_rl <- tryCatch(fit_glmnet_cv(Xtr, ytr, alpha = 1, foldid = foldid, relax = TRUE), error = function(e) NULL)
  ph_rl <- if (is.null(cv_rl)) ph_m else predict_glmnet_resp(cv_rl, Xva)
  out$RelaxedLASSO <- compute_metrics(ph_rl, yva, piv)

  # PCR rules
  for (rule in c("evgt1","var90","aic","cvdev")) {
    fitp <- tryCatch(fit_pcr_mle(Xtr, ytr, rule, foldid), error = function(e) NULL)
    if (is.null(fitp) || fitp$k < 2) {           # substitution: fallback to MLE if < 2 comps
      out[[paste0("PCR_", rule)]] <- out$MLE
    } else {
      php <- tryCatch(predict_pcr_mle(fitp, Xva), error = function(e) rep(mean(ytr), nrow(Xva)))
      out[[paste0("PCR_", rule)]] <- compute_metrics(php, yva, piv)
    }
  }

  # PLS (stopping), cap 30 comps
  fit_pl <- tryCatch(fit_pls(Xtr, ytr, maxcomp = 30, sparseStop = TRUE), error = function(e) NULL)
  if (is.null(fit_pl)) {
    out$PLS <- out$MLE
  } else {
    php <- tryCatch(predict_pls_resp(fit_pl, Xva), error = function(e) rep(mean(ytr), nrow(Xva)))
    out$PLS <- compute_metrics(php, yva, piv)
  }

  # PLS + LASSO hybrid:
  # To keep projection stable (plsRglm hides scores), we approximate with PCR_cvdev scores fed to LASSO
  sel <- tryCatch(select_pcr_components(Xtr, ytr, rule = "cvdev", foldid = foldid), error = function(e) NULL)
  if (is.null(sel) || sel$k < 2) {
    out$PLS_LASSO <- out$MLE
  } else {
    Ztr <- sel$pr$x[, seq_len(sel$k), drop = FALSE]
    cvZ <- tryCatch(cv.glmnet(Ztr, ytr, family = "binomial", alpha = 1,
                              foldid = foldid, type.measure = "deviance"),
                    error = function(e) NULL)
    if (is.null(cvZ)) {
      out$PLS_LASSO <- out$MLE
    } else {
      Zva <- scale(Xva, center = sel$pr$center, scale = sel$pr$scale) %*% sel$pr$rotation
      Zva <- Zva[, seq_len(sel$k), drop = FALSE]
      php <- as.numeric(predict(cvZ, newx = Zva, type = "response", s = "lambda.min"))
      out$PLS_LASSO <- compute_metrics(php, yva, piv)
    }
  }

  bind_rows(out, .id = "method")
}

# ---------- Ranking (paper’s rounding rules) ---------------------------------
rank_with_rounding <- function(df) {
  df %>%
    mutate(
      r_auc   = rank(-round(auc,   3), ties.method = "min"),
      r_brier = rank( round(brier, 3), ties.method = "min"),
      r_cals  = rank( abs(round(cal_slope, 2) - 1), ties.method = "min"),
      r_rmspe = rank( round(rMSPE, 3), ties.method = "min"),
      r_mape  = rank( round(MAPE,  3), ties.method = "min")
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
    res_iter <- vector("list", iters)
    for (it in seq_len(iters)) {
      seed <- base_seed + s$scn_id * 1000L + it
      dat <- gen_dataset(
        p = s$p, event_frac = s$event_frac, EPV = s$EPV,
        noise_frac = s$noise_frac, sparse = s$sparse, seed = seed
      )
      met <- eval_methods_once(dat$Xtr, dat$ytr, dat$Xva, dat$yva, dat$piv,
                               seed_fold = seed + 13L) %>%
        mutate(iter = it)
      res_iter[[it]] <- met
    }
    out[[i]] <- bind_rows(res_iter) %>%
      mutate(scn_id = s$scn_id,
             EPV = s$EPV, event_frac = s$event_frac, p = s$p,
             noise_frac = s$noise_frac, sparse = s$sparse)
  }
  bind_rows(out)
}
