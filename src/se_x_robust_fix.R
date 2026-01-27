################################################################################
#  DROP-IN FIX FOR se_x ESTIMATION IN 5-PARAMETER MODELS
#  
#  Usage: Source this file AFTER loading model_functions.R and std_curve_functions.R
#         source("se_x_robust_fix.R")
#  
#  This will replace the problematic functions with robust versions.
################################################################################

# =============================================================================
# ROBUST GRADIENT FUNCTIONS
# =============================================================================

#' Safe gradient for Y5 inverse function
#' x = c + b * log(((a-d)/(y-d))^(1/g) - 1)
grad_Y5_robust <- function(y, a, b, c, d, g, tol = 1e-10) {
  # Invalid return structure
  invalid <- list(
    grad_theta = c(a = NA_real_, b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_),
    grad_y = NA_real_,
    valid = FALSE
  )
  
  # Validate inputs
  if (any(is.na(c(y, a, b, c, d, g)))) return(invalid)
  if (g <= 0) return(invalid)
  
  # y must be strictly between asymptotes
  y_min <- min(a, d) + tol
  y_max <- max(a, d) - tol
  if (y <= y_min || y >= y_max) return(invalid)
  
  # Compute ratio = (a-d)/(y-d)
  denom <- y - d
  if (abs(denom) < tol) return(invalid)
  
  numer <- a - d
  ratio <- numer / denom
  if (ratio <= 0) return(invalid)
  
  # ratio^(1/g) 
  inv_g <- 1 / g
  ratio_pow <- ratio^inv_g
  
  # H = ratio^(1/g) - 1, must be positive for log
  H <- ratio_pow - 1
  if (H <= tol) return(invalid)
  
  # Compute gradients
  log_H <- log(H)
  log_ratio <- log(ratio)
  
  # ∂x/∂a = b / (g * H * (a - d))
  da <- b / (g * H * numer)
  
  # ∂x/∂b = log(H)
  db <- log_H
  
  # ∂x/∂c = 1
  dc <- 1
  
  # ∂x/∂d - derived via chain rule
  # d(ratio)/dd = (a-y)/(y-d)^2
  # Then: ∂x/∂d = b * ratio^(1/g) * (a-y) / (g * H * (a-d) * (y-d))
  dd <- b * ratio_pow * (a - y) / (g * H * numer * denom)
  
  # ∂x/∂g = -b * ratio^(1/g) * log(ratio) / (g^2 * H)
  dg <- -b * ratio_pow * log_ratio / (g^2 * H)
  
  # ∂x/∂y = -b * ratio^(1/g) / (g * H * (y - d))
  dy <- -b * ratio_pow / (g * H * denom)
  
  # Final validity check
  grad_theta <- c(a = da, b = db, c = dc, d = dd, g = dg)
  if (any(!is.finite(c(grad_theta, dy)))) return(invalid)
  
  list(
    grad_theta = grad_theta,
    grad_y = dy,
    valid = TRUE
  )
}

#' Safe gradient for Yd5 inverse function  
#' x = c - (1/b) * (log(((y-a)/(d-a))^(-g) - 1) - log(g))
grad_Yd5_robust <- function(y, a, b, c, d, g, tol = 1e-10) {
  invalid <- list(
    grad_theta = c(a = NA_real_, b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_),
    grad_y = NA_real_,
    valid = FALSE
  )
  
  if (any(is.na(c(y, a, b, c, d, g)))) return(invalid)
  if (g <= 0 || b <= 0) return(invalid)
  
  y_min <- min(a, d) + tol
  y_max <- max(a, d) - tol
  if (y <= y_min || y >= y_max) return(invalid)
  
  # ratio = (y-a)/(d-a), should be in (0,1) for typical case
  numer <- y - a
  denom <- d - a
  if (abs(denom) < tol) return(invalid)
  
  ratio <- numer / denom
  if (ratio <= tol || ratio >= 1 - tol) return(invalid)
  
  # Q = ratio^(-g) - 1
  ratio_neg_g <- ratio^(-g)
  Q <- ratio_neg_g - 1
  if (Q <= tol) return(invalid)
  
  log_Q <- log(Q)
  log_g <- log(g)
  log_ratio <- log(ratio)
  
  # ∂x/∂a
  # d(ratio)/da = -(d-y)/(d-a)^2 ... wait, let me recalculate
  # ratio = (y-a)/(d-a)
  # d(ratio)/da = [(-1)(d-a) - (y-a)(-1)]/(d-a)^2 = (-d+a+y-a)/(d-a)^2 = (y-d)/(d-a)^2
  # Then chain rule through ratio^(-g) and Q
  da <- g * ratio^(-g-1) * (y - d) / (b * Q * denom^2)
  
  # ∂x/∂b = (log(Q) - log(g)) / b^2
  db <- (log_Q - log_g) / (b^2)
  
  # ∂x/∂c = 1
  dc <- 1
  
  # ∂x/∂d
  # d(ratio)/dd = -(y-a)/(d-a)^2
  dd <- -g * ratio^(-g-1) * numer / (b * Q * denom^2)
  
  # ∂x/∂g = (1/b) * (log(ratio) * ratio^(-g) / Q + 1/g)
  dg <- (1/b) * (log_ratio * ratio_neg_g / Q + 1/g)
  
  # ∂x/∂y = g * ratio^(-g-1) / (b * Q * (d - a))
  dy <- g * ratio^(-g-1) / (b * Q * denom)
  
  grad_theta <- c(a = da, b = db, c = dc, d = dd, g = dg)
  if (any(!is.finite(c(grad_theta, dy)))) return(invalid)
  
  list(
    grad_theta = grad_theta,
    grad_y = dy,
    valid = TRUE
  )
}

#' Safe gradient for Y5 with fixed lower asymptote
grad_inv_Y5_fixed_robust <- function(y, fixed_a, b, c, d, g, tol = 1e-10) {
  result <- grad_Y5_robust(y, fixed_a, b, c, d, g, tol)
  if (!result$valid) {
    return(c(b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_))
  }
  # Remove 'a' from gradient
  result$grad_theta[c("b", "c", "d", "g")]
}

#' Safe gradient for Yd5 with fixed lower asymptote  
grad_inv_Yd5_fixed_robust <- function(y, fixed_a, b, c, d, g, tol = 1e-10) {
  result <- grad_Yd5_robust(y, fixed_a, b, c, d, g, tol)
  if (!result$valid) {
    return(c(b = NA_real_, c = NA_real_, d = NA_real_, g = NA_real_))
  }
  result$grad_theta[c("b", "c", "d", "g")]
}

#' Safe dy/dx for Y5 with fixed a
grad_y_Y5_fixed_robust <- function(y, fixed_a, b, d, g, tol = 1e-10) {
  # We need c for full calculation, but it doesn't affect dy
  result <- grad_Y5_robust(y, fixed_a, b, 0, d, g, tol)  # c=0 is placeholder
  if (!result$valid) return(NA_real_)
  result$grad_y
}

#' Safe dy/dx for Yd5 with fixed a
grad_y_Yd5_fixed_robust <- function(y, fixed_a, b, d, g, tol = 1e-10) {
  result <- grad_Yd5_robust(y, fixed_a, b, 0, d, g, tol)
  if (!result$valid) return(NA_real_)
  result$grad_y
}

# =============================================================================
# NUMERICAL GRADIENT FALLBACK
# =============================================================================

#' Numerical gradient using central differences
numerical_gradient_5param <- function(model, y, params, fixed_a = NULL, h = 1e-6) {
  # Choose appropriate inverse function
  if (model == "Y5") {
    if (!is.null(fixed_a)) {
      inv_fn <- function(p) inv_Y5_fixed(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
      param_names <- c("b", "c", "d", "g")
      params_use <- params[param_names]
    } else {
      inv_fn <- function(p) inv_Y5(y, p["a"], p["b"], p["c"], p["d"], p["g"])
      param_names <- c("a", "b", "c", "d", "g")
      params_use <- params[param_names]
    }
  } else if (model == "Yd5") {
    if (!is.null(fixed_a)) {
      inv_fn <- function(p) inv_Yd5_fixed(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
      param_names <- c("b", "c", "d", "g")
      params_use <- params[param_names]
    } else {
      inv_fn <- function(p) inv_Yd5(y, p["a"], p["b"], p["c"], p["d"], p["g"])
      param_names <- c("a", "b", "c", "d", "g")
      params_use <- params[param_names]
    }
  } else {
    return(list(grad_theta = NULL, grad_y = NA_real_, valid = FALSE))
  }
  
  # Base value
  x0 <- inv_fn(params_use)
  if (is.na(x0) || !is.finite(x0)) {
    return(list(
      grad_theta = setNames(rep(NA_real_, length(param_names)), param_names),
      grad_y = NA_real_,
      valid = FALSE
    ))
  }
  
  # Gradient w.r.t. parameters
  grad_theta <- numeric(length(param_names))
  names(grad_theta) <- param_names
  
  for (i in seq_along(params_use)) {
    p_plus <- p_minus <- params_use
    p_plus[i] <- params_use[i] + h
    p_minus[i] <- params_use[i] - h
    
    x_plus <- inv_fn(p_plus)
    x_minus <- inv_fn(p_minus)
    
    if (is.finite(x_plus) && is.finite(x_minus)) {
      grad_theta[i] <- (x_plus - x_minus) / (2 * h)
    } else if (is.finite(x_plus)) {
      grad_theta[i] <- (x_plus - x0) / h
    } else if (is.finite(x_minus)) {
      grad_theta[i] <- (x0 - x_minus) / h
    } else {
      grad_theta[i] <- NA_real_
    }
  }
  
  # Gradient w.r.t. y
  y_plus <- y + h
  y_minus <- y - h
  
  if (model == "Y5") {
    if (!is.null(fixed_a)) {
      x_y_plus <- inv_Y5_fixed(y_plus, fixed_a, params["b"], params["c"], params["d"], params["g"])
      x_y_minus <- inv_Y5_fixed(y_minus, fixed_a, params["b"], params["c"], params["d"], params["g"])
    } else {
      x_y_plus <- inv_Y5(y_plus, params["a"], params["b"], params["c"], params["d"], params["g"])
      x_y_minus <- inv_Y5(y_minus, params["a"], params["b"], params["c"], params["d"], params["g"])
    }
  } else {
    if (!is.null(fixed_a)) {
      x_y_plus <- inv_Yd5_fixed(y_plus, fixed_a, params["b"], params["c"], params["d"], params["g"])
      x_y_minus <- inv_Yd5_fixed(y_minus, fixed_a, params["b"], params["c"], params["d"], params["g"])
    } else {
      x_y_plus <- inv_Yd5(y_plus, params["a"], params["b"], params["c"], params["d"], params["g"])
      x_y_minus <- inv_Yd5(y_minus, params["a"], params["b"], params["c"], params["d"], params["g"])
    }
  }
  
  if (is.finite(x_y_plus) && is.finite(x_y_minus)) {
    grad_y <- (x_y_plus - x_y_minus) / (2 * h)
  } else if (is.finite(x_y_plus)) {
    grad_y <- (x_y_plus - x0) / h
  } else if (is.finite(x_y_minus)) {
    grad_y <- (x0 - x_y_minus) / h
  } else {
    grad_y <- NA_real_
  }
  
  all_valid <- all(is.finite(grad_theta)) && is.finite(grad_y)
  
  list(
    grad_theta = grad_theta,
    grad_y = grad_y,
    valid = all_valid
  )
}

# =============================================================================
# REPLACEMENT FOR make_inv_and_grad_fixed
# =============================================================================

#' Robust version of make_inv_and_grad_fixed for 5-parameter models
make_inv_and_grad_fixed_robust <- function(model, y, fixed_a) {
  switch(model,
    Y4 = list(
      inv = function(p) inv_Y4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad = function(p) grad_inv_Y4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad_y = function(p) grad_y_Y4_fixed(y, fixed_a, p["b"], p["d"])
    ),
    Yd4 = list(
      inv = function(p) inv_Yd4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad = function(p) grad_inv_Yd4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad_y = function(p) grad_y_Yd4_fixed(y, fixed_a, p["b"], p["c"], p["d"])
    ),
    Ygomp4 = list(
      inv = function(p) inv_Ygomp4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad = function(p) grad_inv_Ygomp4_fixed(y, fixed_a, p["b"], p["c"], p["d"]),
      grad_y = function(p) grad_y_Ygomp4_fixed(y, fixed_a, p["b"], p["d"])
    ),
    Y5 = list(
      inv = function(p) inv_Y5_fixed(y, fixed_a, p["b"], p["c"], p["d"], p["g"]),
      grad = function(p) {
        # Try robust analytic first
        result <- grad_Y5_robust(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
        if (result$valid) {
          return(result$grad_theta[c("b", "c", "d", "g")])
        }
        # Fallback to numerical
        num_result <- numerical_gradient_5param("Y5", y, p, fixed_a)
        return(num_result$grad_theta)
      },
      grad_y = function(p) {
        result <- grad_Y5_robust(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
        if (result$valid) return(result$grad_y)
        num_result <- numerical_gradient_5param("Y5", y, p, fixed_a)
        return(num_result$grad_y)
      }
    ),
    Yd5 = list(
      inv = function(p) inv_Yd5_fixed(y, fixed_a, p["b"], p["c"], p["d"], p["g"]),
      grad = function(p) {
        result <- grad_Yd5_robust(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
        if (result$valid) {
          return(result$grad_theta[c("b", "c", "d", "g")])
        }
        num_result <- numerical_gradient_5param("Yd5", y, p, fixed_a)
        return(num_result$grad_theta)
      },
      grad_y = function(p) {
        result <- grad_Yd5_robust(y, fixed_a, p["b"], p["c"], p["d"], p["g"])
        if (result$valid) return(result$grad_y)
        num_result <- numerical_gradient_5param("Yd5", y, p, fixed_a)
        return(num_result$grad_y)
      }
    ),
    stop("Unsupported model")
  )
}

# =============================================================================
# REPLACEMENT FOR propagate_error_analytic
# =============================================================================

#' Robust error propagation following Daly et al. (2005)
#' 
#' This version handles numerical issues in 5-parameter models by:
#' 1. Using safe analytic gradients with validity checking
#' 2. Falling back to numerical gradients when analytic fails
#' 3. Returning NA for se_x when computation is not possible
propagate_error_analytic_robust <- function(model,
                                            fit,
                                            y,
                                            se_y = 0,
                                            fixed_a,
                                            verbose = FALSE) {
  # Extract coefficients & covariance
  theta <- coef(fit)
  vcov_mat <- vcov(fit)
  
  # Helper for returning invalid result
  invalid_result <- function(x_hat) {
    list(
      x_est = x_hat,
      se_x = NA_real_,
      var_x = NA_real_,
      grad_theta = NULL,
      grad_y = NA_real_
    )
  }
  
  # Compute inverse (point estimate)
  if (!is.null(fixed_a)) {
    inv_and_grad <- make_inv_and_grad_fixed_robust(model, y, fixed_a)
    x_hat <- inv_and_grad$inv(theta)
    
    if (is.na(x_hat) || !is.finite(x_hat)) {
      return(invalid_result(x_hat))
    }
    
    grad_theta <- inv_and_grad$grad(theta)
    grad_y <- inv_and_grad$grad_y(theta)
    
  } else {
    # Non-fixed case
    x_hat <- switch(model,
      Y4 = inv_Y4(y, theta["a"], theta["b"], theta["c"], theta["d"]),
      Yd4 = inv_Yd4(y, theta["a"], theta["b"], theta["c"], theta["d"]),
      Ygomp4 = inv_Ygomp4(y, theta["a"], theta["b"], theta["c"], theta["d"]),
      Y5 = inv_Y5(y, theta["a"], theta["b"], theta["c"], theta["d"], theta["g"]),
      Yd5 = inv_Yd5(y, theta["a"], theta["b"], theta["c"], theta["d"], theta["g"]),
      stop("Unsupported model")
    )
    
    if (is.na(x_hat) || !is.finite(x_hat)) {
      return(invalid_result(x_hat))
    }
    
    # Get gradients - use robust versions for 5-param models
    if (model %in% c("Y5", "Yd5")) {
      if (model == "Y5") {
        grads <- grad_Y5_robust(y, theta["a"], theta["b"], theta["c"], theta["d"], theta["g"])
      } else {
        grads <- grad_Yd5_robust(y, theta["a"], theta["b"], theta["c"], theta["d"], theta["g"])
      }
      
      if (!grads$valid) {
        # Fallback to numerical
        if (verbose) message("Using numerical gradient for ", model, " at y = ", round(y, 4))
        grads <- numerical_gradient_5param(model, y, theta, NULL)
      }
      
      if (!grads$valid) {
        return(invalid_result(x_hat))
      }
      
      grad_theta <- grads$grad_theta
      grad_y <- grads$grad_y
      
    } else {
      # 4-parameter models - use existing functions
      grads <- switch(model,
        Y4 = grad_Y4(y, theta["a"], theta["b"], theta["c"], theta["d"]),
        Yd4 = grad_Yd4(y, theta["a"], theta["b"], theta["c"], theta["d"]),
        Ygomp4 = grad_Ygomp4(y, theta["a"], theta["b"], theta["c"], theta["d"])
      )
      grad_theta <- grads$grad_theta
      grad_y <- grads$grad_y
    }
  }
  
  # Check gradient validity
  if (any(is.na(grad_theta)) || is.na(grad_y) || 
      any(!is.finite(grad_theta)) || !is.finite(grad_y)) {
    if (verbose) message("Invalid gradient for y = ", round(y, 4))
    return(invalid_result(x_hat))
  }
  
  # Ensure gradient names match vcov column order
  grad_theta <- grad_theta[colnames(vcov_mat)]
  
  # Delta-method variance calculation
  var_par <- as.numeric(t(grad_theta) %*% vcov_mat %*% grad_theta)
  var_y <- (grad_y^2) * (se_y^2)
  var_x <- var_par + var_y
  
  # Handle numerical issues
  if (is.na(var_x) || !is.finite(var_x) || var_x < 0) {
    if (verbose) message("Invalid variance for y = ", round(y, 4))
    return(invalid_result(x_hat))
  }
  
  se_x <- sqrt(var_x)
  
  list(
    x_est = x_hat,
    se_x = se_x,
    var_x = var_x,
    grad_theta = grad_theta,
    grad_y = grad_y
  )
}

# =============================================================================
# APPLY THE FIX
# =============================================================================

# Replace the original functions with robust versions
if (exists("propagate_error_analytic")) {
  # Backup original
  propagate_error_analytic_original <- propagate_error_analytic
}

# Install robust version
propagate_error_analytic <- propagate_error_analytic_robust

if (exists("make_inv_and_grad_fixed")) {
  make_inv_and_grad_fixed_original <- make_inv_and_grad_fixed
}

make_inv_and_grad_fixed <- make_inv_and_grad_fixed_robust

message("Robust se_x estimation functions installed successfully.")
message("Original functions backed up as: propagate_error_analytic_original, make_inv_and_grad_fixed_original")
