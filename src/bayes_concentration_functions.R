# ============================================================
# Model string factory — curve params are DATA, only x is sampled
# Uses dunif prior on x instead of hierarchical dnorm
# ============================================================
# get_jags_calibration_model <- function(model_name) {
#   model_name <- trimws(as.character(model_name))
#   model_string <- switch(
#     model_name,
#     
#     # 5PL symmetric: d + (a - d) / (1 + exp((x - c)/b))^g
#     "Y5" = "
#     model {
#       for (i in 1:N) {
#         y[i] ~ dt(mu[i], tau, nu)
#         mu[i] <- d + (a - d) * pow(1 + exp((x[i] - c) / b), -g)
#         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
#       }
#       sigma ~ dunif(0, sigma_upper)
#       tau   <- pow(sigma, -2)
#       nu    ~ dunif(2, 30)
#     }
#     ",
#     
#     # 4PL symmetric: d + (a - d) / (1 + exp((x - c)/b))
#     "Y4" = "
#     model {
#       for (i in 1:N) {
#         y[i] ~ dt(mu[i], tau, nu)
#         mu[i] <- d + (a - d) / (1 + exp((x[i] - c) / b))
#         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
#       }
#       sigma ~ dunif(0, sigma_upper)
#       tau   <- pow(sigma, -2)
#       nu    ~ dunif(2, 30)
#     }
#     ",
#     
#     # 5PL log-logistic: a + (d - a) * (1 + g * exp(-b*(x - c)))^(-1/g)
#     "Yd5" = "
#     model {
#       for (i in 1:N) {
#         y[i] ~ dt(mu[i], tau, nu)
#         mu[i] <- a + (d - a) * pow(1 + g * exp(-b * (x[i] - c)), -1/g)
#         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
#       }
#       sigma ~ dunif(0, sigma_upper)
#       tau   <- pow(sigma, -2)
#       nu    ~ dunif(2, 30)
#     }
#     ",
#     
#     # 4PL log-logistic: a + (d - a) / (1 + exp(-b*(x - c)))
#     "Yd4" = "
#     model {
#       for (i in 1:N) {
#         y[i] ~ dt(mu[i], tau, nu)
#         mu[i] <- a + (d - a) / (1 + exp(-b * (x[i] - c)))
#        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
#       }
#       sigma ~ dunif(0, sigma_upper)
#       tau   <- pow(sigma, -2)
#       nu    ~ dunif(2, 30)
#     }
#     ",
#     
#     # Gompertz: a + (d - a) * exp(-exp(-b*(x - c)))
#     "Ygomp4" = "
#     model {
#       for (i in 1:N) {
#         y[i] ~ dt(mu[i], tau, nu)
#         mu[i] <- a + (d - a) * exp(-exp(-b * (x[i] - c)))
#         x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
#       }
#       sigma ~ dunif(0, sigma_upper)
#       tau   <- pow(sigma, -2)
#       nu    ~ dunif(2, 30)
#     }
#     ",
#     
#     stop("Unsupported model_name: ", model_name)
#   )
#   return(model_string)
# }
# 
# # ============================================================
# # Build the forward function matching each model name
# # ============================================================
# get_forward_function <- function(model_name) {
#   model_name <- trimws(as.character(model_name))
#   switch(
#     model_name,
#     "Y5"    = function(x, p) p["d"] + (p["a"] - p["d"]) * (1 + exp((x - p["c"]) / p["b"]))^(-p["g"]),
#     "Y4"    = function(x, p) p["d"] + (p["a"] - p["d"]) / (1 + exp((x - p["c"]) / p["b"])),
#     "Yd5"   = function(x, p) p["a"] + (p["d"] - p["a"]) * (1 + p["g"] * exp(-p["b"] * (x - p["c"])))^(-1 / p["g"]),
#     "Yd4"   = function(x, p) p["a"] + (p["d"] - p["a"]) / (1 + exp(-p["b"] * (x - p["c"]))),
#     "Ygomp4"= function(x, p) p["a"] + (p["d"] - p["a"]) * exp(-exp(-p["b"] * (x - p["c"]))),
#     stop("Unsupported model_name: ", model_name)
#   )
# }
# 
# # ============================================================
# # Numerically invert the forward curve at observed y values
# # to get good starting x values
# # ============================================================
# get_x_init <- function(model_name, fit, params, y_obs, x_min, x_max, conc_range) {
#   fwd <- get_forward_function(model_name)
#   
#   # --- Numerical inversion (unchanged) ---
#   x_init <- vapply(y_obs, function(yi) {
#     result <- tryCatch(
#       uniroot(
#         function(x) fwd(x, params) - yi,
#         interval = c(x_min, x_max),
#         tol = 1e-8
#       )$root,
#       error = function(e) NA_real_
#     )
#     return(result)
#   }, numeric(1))
#   
#   # Fall back to midpoint for any failures
#   x_init[is.na(x_init)] <- (x_min + x_max) / 2
#   
#   # Clamp inside bounds with small margin
#   margin <- (x_max - x_min) * 0.001
#   x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
#   
#   # --- Compute slope at each x_init ---
#   slope <- vapply(x_init, function(xi) {
#     h <- 1e-5
#     (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
#   }, numeric(1))
#   
#   # --- Compute maximum slope across the curve (reference) ---
#   x_grid <- seq(x_min, x_max, length.out = 500)
#   slope_grid <- vapply(x_grid, function(xi) {
#     h <- 1e-5
#     abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
#   }, numeric(1))
#   max_slope <- max(slope_grid, na.rm = TRUE)
#   
#   # --- Slope ratio: 0 = flat asymptote, 1 = steepest part ---
#   slope_ratio <- abs(slope) / max_slope
#   slope_ratio <- pmin(slope_ratio, 1.0)
#   
#   # --- Tiered variance based on slope ratio ---
#   # Informative (mid-curve): delta-method variance
#   resid_var <- var(resid(fit))
#   delta_var <- (resid_var / slope)^2
#   
#   # Defensive bounds
#   min_var <- (conc_range * 0.001)^2
#   max_var <- (conc_range * 2)^2
#   delta_var <- pmin(pmax(delta_var, min_var), max_var)
#   delta_var[is.na(delta_var)] <- max_var
#   
#   # Weakly informative (asymptote): wide variance spanning the full range
#   wide_var <- (conc_range * 1.0)^2
#   
#   # --- Smooth blending via slope_ratio ---
#   # slope_ratio near 1 -> trust delta method (informative)
#   # slope_ratio near 0 -> use wide prior (weakly informative)
#   # Use a sigmoidal weight so transition is smooth
#   slope_threshold <- 0.15   # below this, prior becomes wide
#   slope_steepness <- 20     # controls transition sharpness
#   weight_informative <- 1 / (1 + exp(-slope_steepness * (slope_ratio - slope_threshold)))
#   
#   per_sample_variance <- weight_informative * delta_var +
#     (1 - weight_informative) * wide_var
#   
#   # --- Flag samples for which the prior is essentially uninformative ---
#   prior_type <- ifelse(weight_informative > 0.5, "delta_method", "wide_prior")
#   
#   return(list(
#     x_init = x_init,
#     per_sample_variance = per_sample_variance,
#     slope_ratio = slope_ratio,
#     weight_informative = weight_informative,
#     prior_type = prior_type
#   ))
# }
# # ============================================================
# # Main runner
# # ============================================================
# run_jags_predicted_concentration <- function(
#     model_name,
#     fit,
#     plate_standards,
#     plate_samples,
#     fixed_constraint = NULL,
#     response_variable,
#     adapt_steps    = 500,
#     burn_in_steps  = 5000,
#     num_saved_steps = 20000,
#     thin_steps     = 2,
#     n_chains       = 3,
#     verbose        = TRUE
# ) {
#   model_name <- trimws(as.character(model_name))
#   
#   # ----------------------------------------------------------
#   # 1. Extract NLS parameters
#   # ----------------------------------------------------------
#   params <- coef(fit)
#   resid_sigma <- var(resid(fit)) #summary(fit)$sigma
#   
#   # Override 'a' if a fixed constraint is supplied
#   if (!is.null(fixed_constraint)) {
#     params["a"] <- fixed_constraint
#   }
#   
#   if (verbose) {
#     cat("Curve parameters (fixed as data):\n")
#     print(params)
#   }
#   
#   # ----------------------------------------------------------
#   # 2. Concentration bounds — wide enough for both asymptotes
#   # ----------------------------------------------------------
#   conc_range <- max(plate_standards$concentration) - min(plate_standards$concentration)
#   # x_min <- min(plate_standards$concentration) - conc_range * 1.0
#   # x_max <- max(plate_standards$concentration) + conc_range * 1.0
#   x_min <- min(plate_standards$concentration) - conc_range * 10.0
#   x_max <- max(plate_standards$concentration) + conc_range * 10.0
#   
#   # ----------------------------------------------------------
#   # 3. Compute good initial x values via numerical inversion
#   # ----------------------------------------------------------
#   y_obs <- plate_samples[[response_variable]]
#   # x_init_list <- get_x_init(model_name, fit, params, y_obs, x_min, x_max, conc_range)
#   # x_init <- x_init_list$x_init
#   # per_sample_variance <- x_init_list$per_sample_variance
#   # 
#   x_init_list <- get_x_init(model_name, fit, params, y_obs, x_min, x_max, conc_range)
#   x_init              <- x_init_list$x_init
#   per_sample_variance <- x_init_list$per_sample_variance
#   
#   # For diagnostics
#   if (verbose) {
#     cat("Slope ratio range:", round(range(x_init_list$slope_ratio), 4), "\n")
#     cat("Prior type counts:\n")
#     print(table(x_init_list$prior_type))
#     cat("Weight range:", round(range(x_init_list$weight_informative), 4), "\n")
#     cat("Initial x range:", round(range(x_init), 4), "\n")
#     cat("Bounds: [", round(x_min, 4), ",", round(x_max, 4), "]\n")
#   }
#   
#   
#   
#   # ----------------------------------------------------------
#   # 4. Build JAGS data list — curve params are DATA
#   # ----------------------------------------------------------
#   # Cap variance: floor to prevent Inf tau, ceiling to prevent zero tau
#   max_var <- (conc_range * 10)^2
#   min_var <- (conc_range * 0.001)^2
#   
#   per_sample_variance <- pmax(per_sample_variance, min_var)  # avoid division by zero
#   per_sample_variance <- pmin(per_sample_variance, max_var)   # avoid near-zero tau
#   per_sample_variance[is.na(per_sample_variance)] <- max_var  # NA -> wide prior
#   tau_x <- 1 / per_sample_variance
#   
#   data_list <- list(
#     y           = y_obs,
#     N           = length(y_obs),
#     a           = unname(params["a"]),
#     b           = unname(params["b"]),
#     c           = unname(params["c"]),
#     d           = unname(params["d"]),
#     x_min       = x_min,
#     x_max       = x_max,
#     sigma_upper = resid_sigma * 5,
#     x_prior =  x_init,
#     tau_x   = tau_x
#   )
#   
#   
#   # Add asymmetry parameter for 5PL models
#   if (model_name %in% c("Y5", "Yd5")) {
#     data_list$g <- unname(params["g"])
#   }
#   
#   # ----------------------------------------------------------
#   # 5. Get model string (no string manipulation needed)
#   # ----------------------------------------------------------
#   model_string <- get_jags_calibration_model(model_name)
#   
#   # ----------------------------------------------------------
#   # 6. Generate initial values per chain
#   # ----------------------------------------------------------
#   init_list <- lapply(seq_len(n_chains), function(chain_id) {
#     list(
#       x     = x_init + rnorm(length(x_init), 0, 0.05),
#       sigma = resid_sigma * runif(1, 0.8, 1.2),
#       nu    = runif(1, 3, 15)
#     )
#   })
#   
#   # ----------------------------------------------------------
#   # 7. MCMC
#   # ----------------------------------------------------------
#   n_iter <- ceiling((num_saved_steps * thin_steps) / n_chains)
#   
#   if (verbose) {
#     cat("\nMCMC configuration:\n")
#     cat("  Chains          :", n_chains, "\n")
#     cat("  Adapt steps     :", adapt_steps, "\n")
#     cat("  Burn-in steps   :", burn_in_steps, "\n")
#     cat("  Iterations/chain:", n_iter, "\n")
#     cat("  Thinning        :", thin_steps, "\n\n")
#     cat("tau_x range:", range(tau_x), "\n")
#     cat("Any NA in tau_x:", any(is.na(tau_x)), "\n")
#     cat("Any Inf in tau_x:", any(is.infinite(tau_x)), "\n")
#     cat("x_prior range:", range(x_init), "\n")
#   }
#   
#   cat("Model String\n")
#   print(textConnection(model_string))
#   
#   jm <- jags.model(
#     textConnection(model_string),
#     data     = data_list,
#     inits    = init_list,
#     n.chains = n_chains,
#     n.adapt  = adapt_steps,
#     quiet    = !verbose
#   )
#   
#   if (verbose) cat("Burning in...\n")
#   update(jm, burn_in_steps)
#   
#   if (verbose) cat("Sampling posterior concentrations...\n")
#   samps <- coda.samples(
#     jm,
#     variable.names = "x",
#     n.iter = n_iter,
#     thin   = thin_steps
#   )
#   
#   # ----------------------------------------------------------
#   # 8. Summarise posteriors
#   # ----------------------------------------------------------
#   chain <- as.matrix(samps)
#   
#   plate_samples$se_robust_concentration     <- apply(chain, 2, sd)
#   #plate_samples$predicted_concentration_lower  <- apply(chain, 2, quantile, 0.025)
#   plate_samples$raw_robust_concentration <- apply(chain, 2, quantile, 0.50)
#   #plate_samples$predicted_concentration_upper  <- apply(chain, 2, quantile, 0.975)
#   
#   # CV on log-scale: sd / |mean|
#   chain_original <- 10^chain
#   plate_samples$pcov_robust_concentration <- apply(chain_original, 2, function(x) {
#     sd(x) / abs(mean(x)) * 100
#   })
#   plate_samples$pcov_robust_concentration <- ifelse(plate_samples$pcov_robust_concentration > 125,
#                                            125,
#                                            plate_samples$pcov_robust_concentration)
#   
#   if (verbose) {
#     cat("\n--- Concentration range check ---\n")
#     cat("Standard curve x range  :",
#         round(range(plate_standards$concentration), 4), "\n")
#     cat("Predicted Mean range  :",
#         round(range(plate_samples$raw_robust_concentration), 4), "\n")
#     cat("CV range                :",
#         round(range(plate_samples$pcov_robust_concentration), 4), "\n")
#   }
#   
#   return(plate_samples)
# }
# 
# # ============================================================
# # Wrapper for both pred_se and sample_se dataframes
# # ============================================================
# 
# run_jags_predicted_concentration_wrapper <- function(best_fit_out, input_df = c("pred_se", "sample_se")) {
#   
#   input_df <- match.arg(input_df)
#   
#   if (input_df == "pred_se") {
#     
#     pred_df <- best_fit_out$pred_se
#     
#     names(pred_df)[names(pred_df) == "x"]    <- "concentration"
#     names(pred_df)[names(pred_df) == "yhat"] <- "mfi"
#     
#     names(best_fit_out)[names(best_fit_out) == "model_name"] <- "best_model_name"
#     
#     plate_samples_bayes <- run_jags_predicted_concentration(
#       model_name        = best_fit_out$best_model_name,
#       fit               = best_fit_out$fit,
#       plate_standards   = pred_df,
#       plate_samples     = pred_df,
#       fixed_constraint  = best_fit_out$fixed_a_result,
#       response_variable = "mfi"
#     )
#     
#   } else {  # sample_se
#     
#     plate_samples_bayes <- run_jags_predicted_concentration(
#       model_name        = best_fit_out$best_fit$best_model_name,
#       fit               = best_fit_out$best_fit$best_fit,
#       plate_standards   = best_fit_out$best_fit$best_data,
#       plate_samples     = best_fit_out$sample_se,
#       fixed_constraint  = best_fit_out$fixed_a_result,
#       response_variable = best_fit_out$response_var
#     )
#   }
#   
#   return(plate_samples_bayes)
# }


### Updated 
# ============================================================
# Model string factory
# ============================================================
get_jags_calibration_model <- function(model_name) {
  switch(
    trimws(model_name),
    "Y5" = "
    model {
      for (i in 1:N) {
        y[i] ~ dt(mu[i], tau, nu)
        mu[i] <- d + (a - d) * pow(1 + exp((x[i] - c) / b), -g)
        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
      }
      sigma ~ dunif(0, sigma_upper)
      tau   <- pow(sigma, -2)
      nu    ~ dunif(2, 30)
    }",
    "Y4" = "
    model {
      for (i in 1:N) {
        y[i] ~ dt(mu[i], tau, nu)
        mu[i] <- d + (a - d) / (1 + exp((x[i] - c) / b))
        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
      }
      sigma ~ dunif(0, sigma_upper)
      tau   <- pow(sigma, -2)
      nu    ~ dunif(2, 30)
    }",
    "Yd5" = "
    model {
      for (i in 1:N) {
        y[i] ~ dt(mu[i], tau, nu)
        mu[i] <- a + (d - a) * pow(1 + g * exp(-b * (x[i] - c)), -1/g)
        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
      }
      sigma ~ dunif(0, sigma_upper)
      tau   <- pow(sigma, -2)
      nu    ~ dunif(2, 30)
    }",
    "Yd4" = "
    model {
      for (i in 1:N) {
        y[i] ~ dt(mu[i], tau, nu)
        mu[i] <- a + (d - a) / (1 + exp(-b * (x[i] - c)))
        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
      }
      sigma ~ dunif(0, sigma_upper)
      tau   <- pow(sigma, -2)
      nu    ~ dunif(2, 30)
    }",
    "Ygomp4" = "
    model {
      for (i in 1:N) {
        y[i] ~ dt(mu[i], tau, nu)
        mu[i] <- a + (d - a) * exp(-exp(-b * (x[i] - c)))
        x[i] ~ dnorm(x_prior[i], tau_x[i]) T(x_min, x_max)
      }
      sigma ~ dunif(0, sigma_upper)
      tau   <- pow(sigma, -2)
      nu    ~ dunif(2, 30)
    }",
    stop("Unsupported model_name: ", model_name)
  )
}

# ============================================================
# Forward function factory
# ============================================================
get_forward_function <- function(model_name) {
  switch(
    trimws(model_name),
    "Y5"     = function(x, p) p["d"] + (p["a"] - p["d"]) * (1 + exp((x - p["c"]) / p["b"]))^(-p["g"]),
    "Y4"     = function(x, p) p["d"] + (p["a"] - p["d"]) / (1 + exp((x - p["c"]) / p["b"])),
    "Yd5"    = function(x, p) p["a"] + (p["d"] - p["a"]) * (1 + p["g"] * exp(-p["b"] * (x - p["c"])))^(-1 / p["g"]),
    "Yd4"    = function(x, p) p["a"] + (p["d"] - p["a"]) / (1 + exp(-p["b"] * (x - p["c"]))),
    "Ygomp4" = function(x, p) p["a"] + (p["d"] - p["a"]) * exp(-exp(-p["b"] * (x - p["c"]))),
    stop("Unsupported model_name: ", model_name)
  )
}

# ============================================================
# Extract named param vector from a best_glance row
# Only includes g for 5PL models
# ============================================================
extract_params <- function(glance_row) {
  p <- c(a = glance_row$a,
         b = glance_row$b,
         c = glance_row$c,
         d = glance_row$d)
  
  if (trimws(glance_row$model_name) %in% c("Y5", "Yd5")) {
    p["g"] <- glance_row$g
  }
  
  return(p)
}

# ============================================================
# Numerical inversion + adaptive prior variance
# ============================================================
compute_x_init <- function(model_name, params, resid_var, y_obs,
                           x_min, x_max, conc_range) {
  fwd <- get_forward_function(model_name)
  
  # Invert curve at each y
  x_init <- vapply(y_obs, function(yi) {
    tryCatch(
      uniroot(function(x) fwd(x, params) - yi,
              interval = c(x_min, x_max), tol = 1e-8)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
  
  x_init[is.na(x_init)] <- (x_min + x_max) / 2
  margin <- (x_max - x_min) * 0.001
  x_init <- pmin(pmax(x_init, x_min + margin), x_max - margin)
  
  # Slope at each x_init
  slope <- vapply(x_init, function(xi) {
    h <- 1e-5
    (fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h)
  }, numeric(1))
  
  # Max slope across curve
  x_grid <- seq(x_min, x_max, length.out = 500)
  max_slope <- max(vapply(x_grid, function(xi) {
    h <- 1e-5
    abs((fwd(xi + h, params) - fwd(xi - h, params)) / (2 * h))
  }, numeric(1)), na.rm = TRUE)
  
  slope_ratio <- pmin(abs(slope) / max_slope, 1.0)
  
  # Delta-method variance
  delta_var <- (resid_var / slope)^2
  min_var <- (conc_range * 0.001)^2
  max_var <- (conc_range * 2)^2
  delta_var <- pmin(pmax(delta_var, min_var), max_var)
  delta_var[is.na(delta_var)] <- max_var
  
  wide_var <- (conc_range * 1.0)^2
  
  # Sigmoidal blend
  weight <- 1 / (1 + exp(-20 * (slope_ratio - 0.15)))
  per_sample_variance <- weight * delta_var + (1 - weight) * wide_var
  
  list(
    x_init              = x_init,
    per_sample_variance = per_sample_variance,
    slope_ratio         = slope_ratio,
    weight_informative  = weight,
    prior_type          = ifelse(weight > 0.5, "delta_method", "wide_prior")
  )
}

# ============================================================
# Main function
#
#   glance_row   : single row from best_glance
#   best_pred_df : rows from best_pred for this glance_row
#                  (x column used for concentration bounds)
#   sample_df    : dataframe to predict on
#                  - pred grid:  best_pred rows, response_col = "yhat"
#                  - samples:    best_sample rows, response_col = "assay_response"
#   response_col : column name holding the response values
# ============================================================
run_jags_predicted_concentration <- function(
    glance_row,
    best_pred_df,
    sample_df,
    response_col,
    adapt_steps     = 500,
    burn_in_steps   = 5000,
    num_saved_steps = 20000,
    thin_steps      = 2,
    n_chains        = 3,
    verbose         = TRUE
) {
  model_name <- trimws(as.character(glance_row$model_name))
  params     <- extract_params(glance_row)
  resid_var  <- glance_row$resid_sample_variance
  
  if (verbose) {
    cat("Model:", model_name, "\n")
    cat("Parameters:\n"); print(params)
    cat("Residual variance:", resid_var, "\n")
  }
  
  # --- Concentration bounds from best_pred$x ---
  conc_range <- max(best_pred_df$x) - min(best_pred_df$x)
  x_min <- min(best_pred_df$x) - conc_range * 10.0
  x_max <- max(best_pred_df$x) + conc_range * 10.0
  
  # --- Initial x + adaptive prior ---
  y_obs <- sample_df[[response_col]]
  
  xinit <- compute_x_init(model_name, params, resid_var, y_obs,
                          x_min, x_max, conc_range)
  
  if (verbose) {
    cat("Slope ratio range:", round(range(xinit$slope_ratio), 4), "\n")
    cat("Prior type counts:\n"); print(table(xinit$prior_type))
    cat("Initial x range:", round(range(xinit$x_init), 4), "\n")
    cat("Bounds: [", round(x_min, 4), ",", round(x_max, 4), "]\n")
  }
  
  # --- Clamp variance ---
  max_var <- (conc_range * 10)^2
  min_var <- (conc_range * 0.001)^2
  psv <- pmin(pmax(xinit$per_sample_variance, min_var), max_var)
  psv[is.na(psv)] <- max_var
  tau_x <- 1 / psv
  
  # --- JAGS data ---
  resid_sigma <- sqrt(resid_var)
  
  data_list <- list(
    y           = y_obs,
    N           = length(y_obs),
    a           = unname(params["a"]),
    b           = unname(params["b"]),
    c           = unname(params["c"]),
    d           = unname(params["d"]),
    x_min       = x_min,
    x_max       = x_max,
    sigma_upper = resid_sigma * 5,
    x_prior     = xinit$x_init,
    tau_x       = tau_x
  )
  
  if (model_name %in% c("Y5", "Yd5")) {
    data_list$g <- unname(params["g"])
  }
  
  # --- MCMC ---
  model_string <- get_jags_calibration_model(model_name)
  
  init_list <- lapply(seq_len(n_chains), function(i) {
    list(
      x     = xinit$x_init + rnorm(length(xinit$x_init), 0, 0.05),
      sigma = resid_sigma * runif(1, 0.8, 1.2),
      nu    = runif(1, 3, 15)
    )
  })
  
  n_iter <- ceiling((num_saved_steps * thin_steps) / n_chains)
  
  if (verbose) {
    cat("MCMC: chains=", n_chains, " adapt=", adapt_steps,
        " burn=", burn_in_steps, " iter/chain=", n_iter,
        " thin=", thin_steps, "\n")
  }
  
  jm <- jags.model(
    textConnection(model_string),
    data     = data_list,
    inits    = init_list,
    n.chains = n_chains,
    n.adapt  = adapt_steps,
    quiet    = !verbose
  )
  update(jm, burn_in_steps)
  
  samps <- coda.samples(jm, variable.names = "x",
                        n.iter = n_iter, thin = thin_steps)
  
  # --- Summarise ---
  chain <- as.matrix(samps)
  
  sample_df$se_robust_concentration  <- apply(chain, 2, sd)
  sample_df$raw_robust_concentration <- apply(chain, 2, quantile, 0.50)
  
  chain_original <- 10^chain
  sample_df$pcov_robust_concentration <- apply(chain_original, 2, function(x) {
    pmin(sd(x) / abs(mean(x)) * 100, 125)
  })
  
  if (verbose) {
    cat("\nStandard curve x range:", round(range(best_pred_df$x), 4), "\n")
    cat("Predicted median range:", round(range(sample_df$raw_robust_concentration), 4), "\n")
    cat("CV range:              ", round(range(sample_df$pcov_robust_concentration), 4), "\n")
  }
  
  return(sample_df)
}
### Shiny Side 

# -----------------------------------------------------------------
# 1. Fetch the job-status table + fill missing combos
# -----------------------------------------------------------------
get_existing_concentration_calc <- function(conn,
                                            project_id,
                                            study_accession,
                                            experiment_accession,
                                            plate_nom) {
  
  query <- glue::glue(
    "SELECT * FROM madi_results.get_job_status(
        {project_id},
        '{study_accession}',
        '{experiment_accession}',
        '{plate_nom}'
    );"
  )
  
  df <- dbGetQuery(conn, query)
  
  # Normalize label for UI
  df$job_status[df$job_status == "partial completion"] <- 
    "partially completed"
  
  return(df)
}
# get_existing_concentration_calc <- function(conn,
#                                             project_id,
#                                             study_accession,
#                                             experiment_accession,
#                                             plate_nom,
#                                             all_methods = c("interpolated", "mcmc_robust"),
#                                             all_scopes  = c("study", "experiment", "plate")) {
#   
#   query <- glue::glue(
#     "SELECT * FROM madi_results.get_job_status_v4({project_id},
#                                                   '{study_accession}',
#                                                   '{experiment_accession}',
#                                                   '{plate_nom}');"
#   )
#   
#   df <- dbGetQuery(conn, query)
#   
#   # Remove unwanted / NULL methods
#   df <- df[!is.na(df$concentration_calc_method) &
#              df$concentration_calc_method != "none", , drop = FALSE]
#   
#   # Map DB status to UI status
#   df$job_status <- ifelse(
#     df$job_status == "partial completion",
#     "partially completed",
#     df$job_status
#   )
#   # Build full grid to guarantee all scope/method combos exist
#   full_grid <- expand.grid(
#     scope                     = all_scopes,
#     concentration_calc_method = all_methods,
#     stringsAsFactors          = FALSE
#   )
#   
#   df$key        <- paste(df$scope, df$concentration_calc_method, sep = "|")
#   full_grid$key <- paste(full_grid$scope, full_grid$concentration_calc_method, sep = "|")
#   
#   missing <- full_grid[!full_grid$key %in% df$key, ]
#   
#   if (nrow(missing) > 0) {
#     missing_rows <- data.frame(
#       scope                     = missing$scope,
#       concentration_calc_method = missing$concentration_calc_method,
#       job_status                = "not begun",
#       incomplete_items          = NA,
#       stringsAsFactors          = FALSE
#     )
#     
#     df <- rbind(df[, names(missing_rows)], missing_rows)
#   }
#   
#   df$key <- NULL
#   
#   return(df)
# }
# get_existing_concentration_calc <- function(conn,
#                                             project_id,
#                                             study_accession,
#                                             experiment_accession,
#                                             plate_nom,
#                                             all_methods = c("interpolated", "mcmc_robust"),
#                                             all_scopes  = c("study", "experiment", "plate")) {
#   
#   query <- glue::glue(
#     "SELECT * FROM madi_results.get_job_status({project_id},
#                                                '{study_accession}',
#                                                '{experiment_accession}',
#                                                '{plate_nom}');"
#   )
#   
#   print(query)
#   df <- dbGetQuery(conn, query)
#   print(df)
#   print(project_id)
#   print(experiment_accession)
#   print(plate_nom)
#   
#   # Remove unwanted method
#   #df <- df[df$concentration_calc_method != "none", , drop = FALSE]
#   df <- df[!is.na(df$concentration_calc_method) &
#          df$concentration_calc_method != "none", , drop = FALSE]
#   
#   # if (nrow(df) == 0) {
#   #   # If nothing exists yet, return full grid with default status
#   #   df <- expand.grid(
#   #     scope                     = all_scopes,
#   #     concentration_calc_method = all_methods,
#   #     job_status = "not begun",
#   #     stringsAsFactors          = FALSE
#   #   )
#   #   return(df)
#   # }
#   
#   full_grid <- expand.grid(
#     scope                     = all_scopes,
#     concentration_calc_method = all_methods,
#     stringsAsFactors          = FALSE
#   )
#   
#   # Build key for comparison
#   df$key        <- paste(df$scope, df$concentration_calc_method, sep = "|")
#   full_grid$key <- paste(full_grid$scope, full_grid$concentration_calc_method, sep = "|")
#   
#   missing <- full_grid[!full_grid$key %in% df$key, ]
#   
#   if (nrow(missing) > 0) {
#     missing_rows <- data.frame(
#       scope                     = missing$scope,
#       concentration_calc_method = missing$concentration_calc_method,
#       job_status                = "not begun",
#       stringsAsFactors          = FALSE
#     )
#     
#     df <- rbind(df[, names(missing_rows)], missing_rows)
#   }
#   
#   df$key <- NULL
#   
#   return(df)
# }



# -----------------------------------------------------------------
# 2. Build a single status badge
# -----------------------------------------------------------------
createStatusBadge <- function(method, existing_concentration_calc, scope) {
  
  row <- existing_concentration_calc[
    existing_concentration_calc$concentration_calc_method == method &
      existing_concentration_calc$scope == scope,
  ]
  
  if (nrow(row) == 0) return(NULL)
  
  # type_status <- row$job_status[1]
  # incomplete  <<- row$incomplete_items[1]
  
  type_status <- row$job_status[1]
  
  incomplete <- if (
    "incomplete_items" %in% names(row) &&
    type_status == "partially completed"
  ) {
    row$incomplete_items[1]
  } else {
    NA
  }
  
  print(incomplete)
  
  # Tooltip if incomplete items exist
  # tooltip <- if (!is.na(incomplete) && incomplete != "") {
  #   paste("Incomplete:", incomplete)
  # } else {
  #   NULL
  # }
  
  tooltip <- if (
    !is.na(incomplete) &&
    nzchar(incomplete)
  ) {
    paste("Incomplete:", incomplete)
  } else {
    NULL
  }
  
  status_style <- switch(type_status,
                         "partially completed" = "background-color: #6f42c1; color: white;",
                         "pending"             = "background-color: #FFA500; color: white;",
                         "completed"           = "background-color: #28a745; color: white;",
                         "not begun"           = "background-color: #dc3545; color: white;",
                         NULL
  )
  
  status_text <- switch(type_status,
                        "partially completed" = tagList(
                          tags$i(class = "fa fa-layer-group"),
                          " Partially Completed"
                        ),
                        "pending" = tagList(
                          tags$i(class = "fa fa-spinner fa-spin"),
                          " Running..."
                        ),
                        "completed" = tagList(
                          tags$i(class = "fa fa-check"),
                          " Completed"
                        ),
                        "not begun" = tagList(
                          tags$i(class = "fa fa-times"),
                          " Not Begun"
                        ),
                        NULL
  )
  
  if (!is.null(status_style) && !is.null(status_text)) {
    span(
      class = "badge",
      title = tooltip,
      style = paste0(
        "padding: 4px 10px; border-radius: 10px; font-size: 12px; ",
        status_style
      ),
      status_text
    )
   
  } else {
    NULL
  }
}
# createStatusBadge <- function(method, existing_concentration_calc, scope) {
#   if (nrow(existing_concentration_calc) == 0) return(NULL)
#   
#   sts <- existing_concentration_calc$job_status[
#     existing_concentration_calc$concentration_calc_method == method &
#       existing_concentration_calc$scope == scope
#   ]
#   
#   if (length(sts) == 0) return(NULL)
#   
#   type_status <- sts[1]
#   
#   status_style <- switch(type_status,
#                          "pending"   = "background-color: #FFA500; color: white;",
#                          "completed" = "background-color: #28a745; color: white;",
#                          "not begun" = "background-color: #dc3545; color: white;",
#                          NULL
#   )
#   
#   status_text <- switch(type_status,
#                         "pending"   = tagList(tags$i(class = "fa fa-spinner fa-spin"), " Running..."),
#                         "completed" = tagList(tags$i(class = "fa fa-check"),           " Completed"),
#                         "not begun" = tagList(tags$i(class = "fa fa-times"),           " Not Begun"),
#                         NULL
#   )
#   
#   if (!is.null(status_style) && !is.null(status_text)) {
#     span(
#       class = "badge",
#       style = paste0(
#         "padding: 4px 10px; border-radius: 10px; font-size: 12px; ",
#         status_style
#       ),
#       status_text
#     )
#   } else {
#     NULL
#   }
# }

# -----------------------------------------------------------------
# 3. Get status for a specific scope + method
# -----------------------------------------------------------------
get_status <- function(existing_concentration_calc, scope, method) {

  sts <- existing_concentration_calc$job_status[
    existing_concentration_calc$concentration_calc_method == method &
      existing_concentration_calc$scope == scope
  ]
  if (length(sts) == 0) return("not begun")
  sts[1]
}

# -----------------------------------------------------------------
# 4. Build the status grid + conditional buttons
# -----------------------------------------------------------------
createStandardCurveConcentrationTypeUI <- function(existing_concentration_calc) {
  concentrationUIRefresher()
  
  ## Method display labels
  method_labels <- c(
    "interpolated" = "Interpolated",
    "mcmc_robust"  = "MCMC Robust"
  )
  
  scope_labels <- c(
    "study"      = "Study (All Experiments)",
    "experiment" = "Experiment (Current)",
    "plate"      = "Plate (Current)"
  )
  
  scope_icons <- c(
    "study"      = "fa-flask",
    "experiment" = "fa-vial",
    "plate"      = "fa-th"
  )
  
  all_methods <- c("interpolated", "mcmc_robust")
  all_scopes  <- c("study", "experiment", "plate")
  
  ## ── Build the status grid as an HTML table ──
  header_cells <- lapply(all_scopes, function(s) {
    tags$th(
      style = "text-align:center; padding:10px 15px; font-size:14px;",
      tags$i(class = paste("fa", scope_icons[s]), style = "margin-right:5px;"),
      scope_labels[s]
    )
  })
  
  body_rows <- lapply(all_methods, function(m) {
    cells <- lapply(all_scopes, function(s) {
      tags$td(
        style = "text-align:center; padding:10px 15px; vertical-align:middle;",
        createStatusBadge(m, existing_concentration_calc, s)
      )
    })
    tags$tr(
      tags$td(
        style = "padding:10px 15px; font-weight:bold; vertical-align:middle;",
        method_labels[m]
      ),
      cells
    )
  })
  
  status_grid <- tags$table(
    class = "table table-bordered",
    style = "width:100%; margin-top:10px; border-radius:8px;",
    tags$thead(
      tags$tr(
        tags$th(style = "padding:10px 15px;", "Method"),
        header_cells
      )
    ),
    tags$tbody(body_rows)
  )
  
  ## ── Scope selector ──
  scope_selector <- radioButtons(
    inputId  = "save_scope",
    label    = "Calculation scope:",
    choices  = c(
      "Current Plate"      = "plate",
      "Current Experiment" = "experiment",
      "All Experiments"    = "study"
    ),
    selected = "study",
    inline   = TRUE
  )
  
  ## ── Buttons section (rendered server-side for conditional logic) ──
  buttons_section <- uiOutput("concentration_buttons_ui")
  
  ## ── Assemble everything ──
  tagList(
    tags$head(tags$style(HTML("
      .conc-btn {
        padding: 12px 30px;
        font-size: 14px;
        line-height: 1.5;
        white-space: normal;
        margin: 5px;
        border-radius: 5px;
        color: white;
        border: none;
        cursor: pointer;
      }
      .conc-btn-green {
        background-color: #7DAF4C;
        border-color: #91CF60;
      }
      .conc-btn-green:hover {
        background-color: #6B9A3F;
      }
      .conc-btn-blue {
        background-color: #4A90D9;
        border-color: #5BA0E9;
      }
      .conc-btn-blue:hover {
        background-color: #3D7DC0;
      }
      .conc-btn-disabled {
        background-color: #cccccc;
        border-color: #bbbbbb;
        color: #666666;
        cursor: not-allowed;
      }
    "))),
    
    wellPanel(
      tags$h4(
        #tags$i(class = "fa fa-table", style = "margin-right:8px;"),
        "Calculation Status of Standard Curves by Concentration Prediction Method"
      ),
      tags$hr(style = "margin-top:5px; margin-bottom:10px;"),
      status_grid,
      tags$hr(),
      scope_selector,
      buttons_section
    )
  )
}


