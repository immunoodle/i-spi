

#' Y5 is a 5-parameter logistic model on the original concentration scale.
Y5 <- function(x, a, b, c, d, g) {
  d + (a - d) / (1 + exp((x - c) / b))^g
}

dydxY5 <- function(x, a, b, c, d, g) {
  u <- exp((x - c) / b)
  -g * (a - d) * u / (b * (1 + u)^(g + 1))
}

d2xY5 <-  function(x, a, b, c, d, g, h = 1e-5) {
  # central difference: f''(x) ≈ (f(x+h) - 2f(x) + f(x-h)) / h^2
  f_x_plus  <- Y5(x + h, a, b, c, d, g)
  f_x       <- Y5(x,     a, b, c, d, g)
  f_x_minus <- Y5(x - h, a, b, c, d, g)
  (f_x_plus - 2 * f_x + f_x_minus) / h^2
}

#' Y4 is a 4-parameter logistic model on the original concentration scale.
Y4 <- function(x, a, b, c, d) {
  d + (a - d) / (1 + exp((x - c) / b))
}

dydxY4 <- function(x, a, b, c, d) {
  u <- exp((x - c) / b)
  -(a - d) * u / (b * (1 + u)^2)
}


d2xY4 <- function(x, a, b, c, d) {
  u <- exp((x - c) / b)
  (a - d) * u * (u - 1) / (b^2 * (1 + u)^3)
}

#' Yd5 is a 5-parameter log-logistic (Hill) model, logistic in log(concentration).
Yd5 <- function(x, a, b, c, d, g) {
  a + (d - a) * (1 + g * exp(-b * (x - c)))^(-1 / g)
}

dydxYd5 <- function(x, a, b, c, d, g) {
  u <- 1 + g * exp(-b * (x - c))
  b * (d - a) * exp(-b * (x - c)) * u^(-1/g - 1)
}


d2xYd5 <-  function(x, a, b, c, d, g, h = 1e-5) {
  # central difference: f''(x) ≈ (f(x+h) - 2f(x) + f(x-h)) / h^2
  f_x_plus  <- Yd5(x + h, a, b, c, d, g)
  f_x       <- Yd5(x,     a, b, c, d, g)
  f_x_minus <- Yd5(x - h, a, b, c, d, g)
  (f_x_plus - 2 * f_x + f_x_minus) / h^2
}

#' Yd4 is a 4-parameter log-logistic (Hill) model, logistic in log(concentration).
Yd4 <- function(x, a, b, c, d) {
  a + (d - a) * (1 + exp(-b * (x - c)))
}

dydxYd4 <- function(x, a, b, c, d) {
  u <- exp(-b * (x - c))
  b * (d - a) * u / (1 + u)^2
}

d2xYd4 <- function (x, a, b, c, d)  {
  b^2 * (d - a) * exp(-(b * (x - c)))
}

#' Gompertz-like 4-parameter
Ygomp4 <- function(x, a, b, c, d) {
  a + (d - a) * exp(-exp(-b * (x - c)))
}

dydxYgomp4 <- function(x, a, b, c, d) {
  u <- exp(-b * (x - c))
  b * (d - a) * u * exp(-u)
}

d2xYgomp4 <- function (x, a, b, c, d) {
  .e2 <- exp(-(b * (x - c)))
  b^2 * (d- a) * .e2 * (.e2 - 1) * exp(-.e2)
}

# Inverse functions
inv_Y5 <- function(y, a, b, c, d, g) {
  c + b * log(((a - d) / (y - d))^(1 / g) - 1)
}

inv_Y5_fixed <- function(y, fixed_a, b, c, d, g) {
  c + b * log(((fixed_a - d) / (y - d))^(1 / g) - 1)
}

dxdyiY5 <- function(y, a, b, c, d, g) {
  ratio <- (a - d) / (y - d)
  b * (1 / g) * ratio^(1 / g) / ((ratio^(1 / g) - 1) * (y - d))
}

inv_Yd5 <- function(y, a, b, c, d, g) {
  c - (1 / b) * (
    log(((y - a) / (d - a))^(-g) - 1) - log(g)
  )
}

inv_Yd5_fixed <- function(y, fixed_a, b, c, d, g) {
  c - (1 / b) * (
    log(((y - fixed_a) / (d - fixed_a))^(-g) - 1) - log(g)
  )
}

dxdyiYd5 <- function(y, a, b, c, d, g) {
  ratio <- (y - a) / (d - a)
  (1 / b) * g * ratio^(-g) / ((ratio^(-g) - 1) * (y - a))
}


# inv_Y4 <- function(y, a, b, c, d) {
#   c + b * log((a - d) / (y - d) - 1)
# }

#' Safe inverse Y4 with bounds checking
#' @param y response value(s)
#' @param a lower asymptote
#' @param b slope parameter
#' @param c inflection point
#' @param d upper asymptote
#' @param tol tolerance for boundary (default 1e-6)
#' @return predicted x value(s), NA for out-of-bounds responses
inv_Y4 <- function(y, a, b, c, d, tol = 1e-6) {
  # Ensure y is strictly between asymptotes
  y_min <- min(a, d) + tol
  y_max <- max(a, d) - tol

  result <- rep(NA_real_, length(y))
  valid <- !is.na(y) & y > y_min & y < y_max

  if (any(valid)) {
    result[valid] <- c + b * log((a - d) / (y[valid] - d) - 1)
  }

  return(result)
}

inv_Y4_fixed <- function(y, fixed_a, b, c, d) {
  c + b * log((fixed_a - d) / (y - d) - 1)
}

dxdyiY4 <- function(y, a, b, c, d) {
  ratio <- (a - d) / (y - d)
  b * ratio / ((ratio - 1) * (y - d))
}

inv_Yd4 <- function(y, a, b, c, d) {
  c * (((d - a) / (y - a)) - 1)^(1 / b)
}

inv_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  c * (((d - fixed_a) / (y - fixed_a)) - 1)^(1 / b)
}

dxdyiYd4 <- function(y, a, b, c, d) {
  inner <- ((d - a) / (y - a)) - 1
  -c * (1 / b) * inner^(1 / b - 1) * (d - a) / (y - a)^2
}

inv_Ygomp4 <- function(y, a, b, c, d) {
  c - (1 / b) * log(-log((y - a) / (d - a)))
}

inv_Ygomp4_fixed <- function(y, fixed_a, b, c, d) {
  c - (1 / b) * log(-log((y - fixed_a) / (d - fixed_a)))
}

dxdyiYgomp4 <- function(y, a, b, c, d) {
  ratio <- (y - a) / (d - a)
  (1 / b) / ((y - a) * log(ratio))
}

## 1. 4‑parameter logistic (Y4)
grad_Y4 <- function(y, a, b, c, d) {
  A  <- (a - d) / (y - d) - 1                # inside the log()
  # ---- partials -------------------------------------------------
  da  <-  b / (A * (y - d))                  # ∂x/∂a
  db  <-  log(A)                             # ∂x/∂b
  dc  <-  1                                   # ∂x/∂c
  dd  <-  b * (a - y) / (A * (y - d)^2)       # ∂x/∂d
  dy  <- -b * (a - d) / (A * (y - d)^2)       # ∂x/∂y
  list(grad_theta = c(a = da, b = db, c = dc, d = dd),
       grad_y     = dy)
}

## 2. 4‑parameter log‑logistic (Yd4)
grad_Yd4 <- function(y, a, b, c, d) {
  Q   <- ( (d - a) / (y - a) ) - 1           # the term inside the power
  p   <- 1 / b
  x   <- c * Q^p                            # (only needed for a few shortcuts)
  # ---- partials -------------------------------------------------
  da  <-  (c / b) * Q^(p - 1) * (d - y) / (y - a)^2
  db  <- -x * log(Q) / b^2
  dc  <-  Q^p                                 # = x / c
  dd  <-  (c / b) * Q^(p - 1) / (y - a)
  dy  <- -(c / b) * Q^(p - 1) * (d - a) / (y - a)^2
  list(grad_theta = c(a = da, b = db, c = dc, d = dd),
       grad_y     = dy)
}

## 3. 4‑parameter Gompertz (Ygomp4)
grad_Ygomp4 <- function(y, a, b, c, d) {
  R   <- (y - a) / (d - a)                 # ratio inside the log‑log
  B   <- -log(R)                           # B = -log(R)  (always >0 for admissible y)
  # ---- partials -------------------------------------------------
  da  <-  1 / (b * B * (y - a))            # ∂x/∂a
  db  <-  log(B) / b^2                      # ∂x/∂b   (note the sign, see derivation)
  dc  <-  1
  dd  <- -1 / (b * B * (d - a))            # ∂x/∂d
  dy  <-  1 / (b * B * (y - a))            # ∂x/∂y
  list(grad_theta = c(a = da, b = db, c = dc, d = dd),
       grad_y     = dy)
}

## 4. 5‑parameter logistic (Y5)
grad_Y5 <- function(y, a, b, c, d, g) {
  H   <- ((a - d) / (y - d))^(1 / g) - 1    # term inside the outer log()
  # ---- partials -------------------------------------------------
  da  <-  b / (g * H * (a - d))
  db  <-  log(H)
  dc  <-  1
  dd  <-  b * ( -1 / (g * H * (y - d))
                - (a - d) / ( (y - d)^2 * H ) )
  dg  <- -b * log( (a - d) / (y - d) ) / (g^2 * H)
  dy  <-  b * (a - d) / (g * H * (y - d)^2)
  list(grad_theta = c(a = da, b = db, c = dc, d = dd, g = dg),
       grad_y     = dy)
}

## 5. 5‑parameter log‑logistic (Yd5)
grad_Yd5 <- function(y, a, b, c, d, g) {
  Q   <- ((y - a) / (d - a))^(-g) - 1      # term that appears inside the log()
  # ---- partials -------------------------------------------------
  da  <-  (1 / (b * Q * g)) *
    ((y - a) / (d - a))^(-g - 1) *
    (1 / (d - a))
  db  <-  (log(g) - log(Q)) / b^2
  dc  <-  1
  dd  <-  (1 / (b * Q * g)) *
    ((y - a) / (d - a))^(-g - 1) *
    (y - a) / (d - a)^2
  dg  <-  (1 / (b * g^2)) * (1 - log(g * Q))
  dy  <-  ((y - a) / (d - a))^(-g - 1) /
    (b * g * Q * (d - a))
  list(grad_theta = c(a = da, b = db, c = dc, d = dd, g = dg),
       grad_y     = dy)
}

### 1a. Y4  (fixed a)
grad_inv_Y4_fixed <- function(y, fixed_a, b, c, d) {
  inside   <- (fixed_a - d) / (y - d) - 1          # the log‑argument
  db <- log(inside)                               # ∂/∂b
  dc <- 1                                         # ∂/∂c
  # ∂/∂d  = b/(fixed_a - y) * ( 1/(fixed_a - d) - 1/(y - d) )
  dd <- (b / (fixed_a - y)) * ( 1/(fixed_a - d) - 1/(y - d) )
  c(b = db, c = dc, d = dd)
}

### 1b. Yd4 (fixed a)
grad_inv_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  inside <- (d - fixed_a) / (y - fixed_a) - 1
  Z      <- inside^(1 / b)                        # the whole expression without the leading c
  db <- -c * Z * log(inside) / (b^2)              # ∂/∂b
  dc <- Z                                         # ∂/∂c
  # ∂/∂d = (c / b) * Z / (d - fixed_a) / inside
  dd <- (c / b) * Z / (d - fixed_a) / inside
  c(b = db, c = dc, d = dd)
}

### 1c. Ygomp4 (fixed a)
grad_inv_Ygomp4_fixed <- function(y, fixed_a, b, c, d) {
  L  <- -log( (y - fixed_a) / (d - fixed_a) )    # = -log(ratio)
  db <-  log(L) / (b^2)                           # ∂/∂b
  dc <-  1                                        # ∂/∂c
  # ∂/∂d = -1 / ( b * L * (d - fixed_a) )
  dd <- -1 / (b * L * (d - fixed_a))
  c(b = db, c = dc, d = dd)
}

### 1d. Y5 (fixed a) – 5‑parameter logistic
grad_inv_Y5_fixed <- function(y, fixed_a, b, c, d, g) {
  ratio   <- (fixed_a - d) / (y - d)                # >0
  ratio_g <- ratio^(1 / g)                         # = ratio^(1/g)
  U       <- ratio_g - 1

  db <- log(U)                                     # ∂/∂b
  dc <- 1                                          # ∂/∂c

  # ∂/∂d = (b / (g * U)) * ratio_g * ( 1/(y-d) - 1/(fixed_a-d) )
  dd <- (b / (g * U)) * ratio_g * ( 1/(y - d) - 1/(fixed_a - d) )

  # ∂/∂g = -(b / g^2) * log(ratio) * (ratio_g) / U
  dg <- -(b / g^2) * log(ratio) * ratio_g / U
  c(b = db, c = dc, d = dd, g = dg)
}

### 1e. Yd5 (fixed a) – 5‑parameter decreasing logistic
grad_inv_Yd5_fixed <- function(y, fixed_a, b, c, d, g) {
  ratio   <- (y - fixed_a) / (d - fixed_a)           # >0
  ratio_g <- ratio^(-g)                             # = ratio^(-g)
  V       <- ratio_g - 1                            # >0

  db <- (log(V) - log(g)) / (b^2)                   # ∂/∂b
  dc <- 1                                           # ∂/∂c

  # ∂/∂d = - (1 / b) * ( ratio_g / V ) * ( g / (d - fixed_a) )
  dd <- - (1 / b) * ( ratio_g / V ) * ( g / (d - fixed_a) )

  # ∂/∂g = (1 / b) * ( (ratio_g / V) * log(ratio) - 1 / g )
  dg <- (1 / b) * ( (ratio_g / V) * log(ratio) - 1 / g )
  c(b = db, c = dc, d = dd, g = dg)
}

### 2a. Y4
grad_y_Y4_fixed <- function(y, fixed_a, b, d) {
  inside <- (fixed_a - d) / (y - d) - 1
  # dx/dy = -b * (fixed_a - d) / ( (y - d)^2 * inside )
  -b * (fixed_a - d) / ((y - d)^2 * inside)
}

### 2b. Yd4
grad_y_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  inside <- (d - fixed_a) / (y - fixed_a) - 1
  # dx/dy = -c/(b) * inside^(1/b - 1) * (d - fixed_a)/(y - fixed_a)^2
  -c / b * inside^(1 / b - 1) * (d - fixed_a) / (y - fixed_a)^2
}

### 2c. Ygomp4
grad_y_Ygomp4_fixed <- function(y, fixed_a, b, d) {
  ratio <- (y - fixed_a) / (d - fixed_a)
  L     <- -log(ratio)                               # L > 0
  # dx/dy = 1 / ( b * L * ratio * (d - fixed_a) )
  1 / (b * L * ratio * (d - fixed_a))
}

### 2d. Y5
grad_y_Y5_fixed <- function(y, fixed_a, b, d, g) {
  ratio   <- (fixed_a - d) / (y - d)                   # >0
  ratio_g <- ratio^(1 / g)                            # = ratio^(1/g)
  U       <- ratio_g - 1
  # dx/dy = b * (1/U) * (-(fixed_a - d)/(g*(y-d)^2)) * ratio^(1/g - 1)
  -b * (fixed_a - d) / (g * (y - d)^2) *
    ratio^(1 / g - 1) / U
}

### 2e. Yd5
grad_y_Yd5_fixed <- function(y, fixed_a, b, d, g) {
  ratio   <- (y - fixed_a) / (d - fixed_a)            # >0
  ratio_g <- ratio^(-g)                               # = ratio^(-g)
  V       <- ratio_g - 1
  # dx/dy = (g/(b*(d - fixed_a))) * ratio^(-g-1) / V
  g / (b * (d - fixed_a)) * ratio^(-g - 1) / V
}


make_inv_and_grad_fixed <- function(model, y, fixed_a) {
  ## Returns a list with three components:
  ##   $inv   – (theta) -> x̂   (the back‑transformed point estimate)
  ##   $grad  – (theta) -> ∂x̂/∂θ   (named vector)
  ##   $grad_y– (theta) -> ∂x̂/∂y   (scalar)
  switch(model,
         Y4 = list(
           inv   = function(p) inv_Y4_fixed(y, fixed_a,
                                            p["b"], p["c"], p["d"]),
           grad  = function(p) grad_inv_Y4_fixed(y, fixed_a,
                                                 p["b"], p["c"], p["d"]),
           grad_y= function(p) grad_y_Y4_fixed(y, fixed_a,
                                               p["b"], p["d"])
         ),
         Yd4 = list(
           inv   = function(p) inv_Yd4_fixed(y, fixed_a,
                                             p["b"], p["c"], p["d"]),
           grad  = function(p) grad_inv_Yd4_fixed(y, fixed_a,
                                                  p["b"], p["c"], p["d"]),
           grad_y= function(p) grad_y_Yd4_fixed(y, fixed_a,
                                                p["b"], p["c"], p["d"])
         ),
         Ygomp4 = list(
           inv   = function(p) inv_Ygomp4_fixed(y, fixed_a,
                                                p["b"], p["c"], p["d"]),
           grad  = function(p) grad_inv_Ygomp4_fixed(y, fixed_a,
                                                     p["b"], p["c"], p["d"]),
           grad_y= function(p) grad_y_Ygomp4_fixed(y, fixed_a,
                                                   p["b"], p["d"])
         ),
         Y5 = list(
           inv   = function(p) inv_Y5_fixed(y, fixed_a,
                                            p["b"], p["c"], p["d"], p["g"]),
           grad  = function(p) grad_inv_Y5_fixed(y, fixed_a,
                                                 p["b"], p["c"], p["d"], p["g"]),
           grad_y= function(p) grad_y_Y5_fixed(y, fixed_a,
                                               p["b"], p["d"], p["g"])
         ),
         Yd5 = list(
           inv   = function(p) inv_Yd5_fixed(y, fixed_a,
                                             p["b"], p["c"], p["d"], p["g"]),
           grad  = function(p) grad_inv_Yd5_fixed(y, fixed_a,
                                                  p["b"], p["c"], p["d"], p["g"]),
           grad_y= function(p) grad_y_Yd5_fixed(y, fixed_a,
                                                p["b"], p["d"], p["g"])
         ),
         stop("Unsupported model"))
}

# a is l_asy (left/lower asymptote)
# b scal
# c xmid
# d is r_asy (right asymptote)
# g is g

## Select the model formulas; the left asymptote can be constrained or not
select_model_formulas <- function(fixed_constraint, response_variable, is_log_response) {
  if (!is.null(fixed_constraint)){
    if (is_log_response) {
      .eps = 0.00005
      fixed_constraint <- log10(fixed_constraint + .eps)
    }
    message("Lower asymptote is fixed at", fixed_constraint)
    fixed_value <- fixed_constraint
    standard_curve_formulas <- list(
      # #mfi ~ r_asy + (((fixed_value) - r_asy)/(1 + exp((log_dilution-xmid)/scal))^g),
      Y5 = as.formula(substitute(lhs ~ d + (((fixed_a) -d)/(1 + exp((concentration-c)/b))^g),
                                 list(lhs = as.name(response_variable), fixed_a = fixed_value)
      ), env = parent.frame()
      ),

      #mfi ~ r_asy + (l_asy - r_asy) * (1 + g * exp(-scal * (log_dilution - xmid)))^(-1/g),
      Yd5 = as.formula(substitute(lhs ~ (fixed_a) + (d - (fixed_a)) * I((1 + g * exp(-b * (concentration - c)))^(-1 / g)),
                                  list(lhs = as.name(response_variable),
                                       fixed_a = fixed_value)
      ) , env = parent.frame()
      ),

      #mfi ~ r_asy + (((fixed_value) - r_asy) / (1 + exp((log_dilution - xmid) / scal))),
      Y4 = as.formula(substitute(lhs ~ d + (((fixed_a) - d) / I((1 + exp((concentration - c)/b)))),
                                 list(lhs = as.name(response_variable), fixed_a = fixed_value)
      ), env = parent.frame()
      ),


      # mfi ~ r_asy + ((fixed_value) - r_asy) / (1 + (log_dilution / xmid)^scal),
      Yd4  = as.formula(substitute(lhs ~ (fixed_a) + (d - (fixed_a)) / I((1 + (concentration / c)^b)),
                                   list(lhs = as.name(response_variable),fixed_a = fixed_value)
      ), env = parent.frame()
      ),

      # Ygomp4 =
      Ygomp4 = as.formula(substitute(lhs ~ (fixed_a) + (d - (fixed_a)) * I(exp(-exp(-b * (concentration - c)))),
                                     list(lhs = as.name(response_variable), fixed_a = fixed_value)
      ), env = parent.frame()
      )


    )

  } else {
    standard_curve_formulas <- list(

      Y5 = as.formula(substitute(lhs ~ d + (a - d)/(I((1 + exp((concentration - c)/b))^g)),
                                 list(lhs = as.name(response_variable))),
                      env = parent.frame()
      ),

      Yd5 = as.formula(substitute(lhs ~ a + (d - a) * I((1 + g * exp(-b * (concentration - c)))^(-1 / g)),
                                  list(lhs = as.name(response_variable))),
                       env = parent.frame()
      ),

      Y4  = as.formula(substitute(lhs ~ d + (a - d) / I(1 + exp((concentration - c) / b)),
                                  list(lhs = as.name(response_variable))),
                       env = parent.frame()
      ),

      Yd4 = as.formula(substitute(lhs ~ a + (d - a) / I(1 + (concentration / c)^b),
                                  list(lhs = as.name(response_variable))),
                       env = parent.frame()
      ),

      Ygomp4 = as.formula(substitute(lhs ~ a + (d - a) * I(exp(-exp(-b * (concentration - c)))),
                                     list(lhs = as.name(response_variable))),
                          env = parent.frame()
      )

      # nls_exp = as.formula(mfi ~ d + (b - d) * I(exp(c * concentration)), env = parent.frame())
    )
  }

  return(standard_curve_formulas)
}

Y5_safe_constraint <- function(data, y_min = 1, y_max, Y5_formula, Y5_free_vars, is_log_response, is_log_concentration, antigen_settings) {
  .eps <- 1e-5
  .slope_max <- 2
  .slope_min <- 0.1
  .g_min <- 0.5
  .g_max <- 5.0

  mid_bounds <- .y_mid_bounds(y_min, y_max)
  mid_low <- as.numeric(mid_bounds["low"])
  mid_high <- as.numeric(mid_bounds["high"])

  # extract parameter names from formula
  formula_vars <- all.vars(Y5_formula)
  a_lower <- antigen_settings$l_asy_min_constraint
  a_upper <- antigen_settings$l_asy_max_constraint
  if (is_log_response) {
    a_lower <- log10(a_lower + .eps)
    a_upper <- log10(a_upper + .eps)
  }

  d_lower <- mid_high / 2
  d_upper <- 2 * mid_high

  midpoint_concentration <- (min(data$concentration) + max(data$concentration))/2
  conc_range <- max(data$concentration) - min(data$concentration)
  c_lower <- midpoint_concentration - (0.5 * conc_range)  # Wider
  c_upper <- midpoint_concentration + (0.5 * conc_range)  # Wi

  b_lower <- 0.1 #.eps
  b_upper <- .slope_max

  g_lower <- .g_min
  g_upper <- .g_max

  if ("a" %in% formula_vars) {
    lower <- c(a = a_lower, b = b_lower, c = c_lower, d = d_lower, g = g_lower)
    upper <- c(a = a_upper, b = b_upper, c = c_upper, d = d_upper, g = g_upper)
  } else {
    lower <- c(b = b_lower, c = c_lower, d = d_lower,g = g_lower)
    upper <- c(b = b_upper, c = c_upper, d = d_lower,g = g_upper)
  }

  return(.make_bounds(Y5_free_vars, lower_vals = lower, upper_vals = upper))

}

Yd5_safe_constraint <- function(data, y_min=1, y_max, Yd5_formula, Yd5_free_vars, is_log_response, is_log_concentration, antigen_settings) {
  .eps <- 1e-5
  .slope_max <- 2
  .g_min <- 0.5
  .g_max <- 5

  mid_bounds <- .y_mid_bounds(y_min, y_max)
  mid_low <- as.numeric(mid_bounds["low"])
  mid_high <- as.numeric(mid_bounds["high"])

  formula_vars <- all.vars(Yd5_formula)

  a_lower <- antigen_settings$l_asy_min_constraint
  a_upper <- antigen_settings$l_asy_max_constraint
  if (is_log_response) {
    a_lower <- log10(a_lower + .eps)
    a_upper <- log10(a_upper + .eps)
  }

  d_lower <- mid_high / 2
  d_upper <- 2 * mid_high

  b_lower <- .eps
  b_upper <- .slope_max

  midpoint_concentration <- (min(data$concentration) + max(data$concentration))/2
  conc_range <- max(data$concentration) - min(data$concentration)
  c_lower <- midpoint_concentration - 0.7 * conc_range
  c_upper <- midpoint_concentration + 0.7 * conc_range

  # g must be != 0; avoid very small or negative values that make base <= 0 for negative g
  g_lower <- .g_min
  g_upper <- .g_max

  if ("a" %in% formula_vars) {
    lower <- c(a = a_lower, b = b_lower, c = c_lower, d = d_lower, g = g_lower)
    upper <- c(a = a_upper, b = b_upper, c = c_upper, d = d_upper, g = g_upper)
  } else {
    lower <- c(b = b_lower, c = c_lower, d = d_lower,g = g_lower)
    upper  <- c(b = b_upper, c = c_upper, d = d_upper,g = g_upper)
  }

  return(.make_bounds(Yd5_free_vars, lower_vals = lower, upper_vals = upper))
}

Y4_safe_constraint <- function(data, y_min=1, y_max, Y4_formula, Y4_free_vars, is_log_response, is_log_concentration, antigen_settings) {
  .eps <- 1e-5
  .slope_max <-  2

  mid_bounds <- .y_mid_bounds(y_min, y_max)
  mid_low <- as.numeric(mid_bounds["low"])
  mid_high <- as.numeric(mid_bounds["high"])

  formula_vars <- all.vars(Y4_formula)

  a_lower <- antigen_settings$l_asy_min_constraint
  a_upper <- antigen_settings$l_asy_max_constraint
  if (is_log_response) {
    a_lower <- log10(a_lower + .eps)
    a_upper <- log10(a_upper + .eps)
  }

  # Constrain midpoint (a + d)/2 to middle90 by bounding a and d so midpoint can lie there.
  # We'll choose ranges so that a in [eps, 2*mid_high] and d in [2*mid_low - a_upper, y_max*2]
  d_lower <- mid_high /2  #.eps
  d_upper <- 2 * mid_high

  b_lower <-  .eps
  b_upper <-   .slope_max

  midpoint_concentration <- (min(data$concentration) + max(data$concentration))/2
  conc_range <- max(data$concentration) - min(data$concentration)
  c_lower <- midpoint_concentration - 0.7 * conc_range
  c_upper <- midpoint_concentration + 0.7 * conc_range

  if ("a" %in% formula_vars) {
    lower <- c(a = a_lower, b = b_lower, c = c_lower, d = d_lower)
    upper <- c(a = a_upper, b = b_upper, c = c_upper, d = d_upper)
  } else {
    lower <- c(b = b_lower, c = c_lower, d = d_lower)
    upper <- c(b = b_upper, c = c_upper, d = d_upper)
  }

  return(.make_bounds(Y4_free_vars, lower_vals = lower, upper_vals = upper))
}

Yd4_safe_constraint <- function(data, y_min=1, y_max, Yd4_formula, Yd4_free_vars, is_log_response, is_log_concentration, antigen_settings) {
  .eps <- 1e-5
  .slope_max <- 2

  mid_bounds <- .y_mid_bounds(y_min, y_max)
  mid_low <- as.numeric(mid_bounds["low"])
  mid_high <- as.numeric(mid_bounds["high"])

  formula_vars <- all.vars(Yd4_formula)

  a_lower <- antigen_settings$l_asy_min_constraint
  a_upper <- antigen_settings$l_asy_max_constraint
  if (is_log_response) {
    a_lower <- log10(a_lower + .eps)
    a_upper <- log10(a_upper + .eps)
  }

  # Constrain midpoint (a + d)/2 to middle90 by bounding a and d so midpoint can lie there.
  # We'll choose ranges so that a in [eps, 2*mid_high] and d in [2*mid_low - a_upper, y_max*2]
  d_lower <- mid_high /2  #.eps
  d_upper <- 2 * mid_high

  b_lower <-  .eps
  b_upper <-   .slope_max

  midpoint_concentration <- (min(data$concentration) + max(data$concentration))/2
  conc_range <- max(data$concentration) - min(data$concentration)
  c_lower <- midpoint_concentration - 0.7 * conc_range
  c_upper <- midpoint_concentration + 0.7 * conc_range

  if ("a" %in% formula_vars) {
    lower <- c(a = a_lower, b = b_lower, c = c_lower, d = d_lower)
    upper <- c(a = a_upper, b = b_upper, c = c_upper, d = d_upper)
  } else {
    lower <- c(b = b_lower, c = c_lower, d = d_lower)
    upper <- c(b = b_upper, c = c_upper, d = d_upper)
  }

  return(.make_bounds(Yd4_free_vars, lower_vals = lower, upper_vals = upper))
}

Ygomp4_safe_constraint <- function(data, y_min=1, y_max, Ygomp4_formula, Ygomp4_free_vars, is_log_response, is_log_concentration, antigen_settings) {
  .eps <- 1e-5
  .slope_max <-  2

  mid_bounds <- .y_mid_bounds(y_min, y_max)
  mid_low <- as.numeric(mid_bounds["low"])
  mid_high <- as.numeric(mid_bounds["high"])

  formula_vars <- all.vars(Ygomp4_formula)

  a_lower <- antigen_settings$l_asy_min_constraint
  a_upper <- antigen_settings$l_asy_max_constraint
  if (is_log_response) {
    a_lower <- log10(a_lower + .eps)
    a_upper <- log10(a_upper + .eps)
  }

  # Constrain midpoint (a + d)/2 to middle90 by bounding a and d so midpoint can lie there.
  # We'll choose ranges so that a in [eps, 2*mid_high] and d in [2*mid_low - a_upper, y_max*2]
  d_lower <- mid_high /2  #.eps
  d_upper <- 2 * mid_high

  b_lower <-  .eps
  b_upper <-   .slope_max

  midpoint_concentration <- (min(data$concentration) + max(data$concentration))/2
  conc_range <- max(data$concentration) - min(data$concentration)
  # # Allow EC50 anywhere in the data range, or slightly beyond
  c_lower <- midpoint_concentration - 0.7 * conc_range
  c_upper <- midpoint_concentration + 0.7 * conc_range

  if ("a" %in% formula_vars) {
    lower <- c(a = a_lower, b = b_lower, c = c_lower, d = d_lower)
    upper <- c(a = a_upper, b = b_upper, c = c_upper, d = d_upper)
  } else {
    lower <- c(b = b_lower, c = c_lower, d = d_lower)
    upper <- c(b = b_upper, c = c_upper, d = d_upper)
  }
  return(.make_bounds(Ygomp4_free_vars, lower_vals = lower, upper_vals = upper))
}
