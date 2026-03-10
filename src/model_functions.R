# ============================================================
#  model_functions.R  — corrected
# ============================================================

# ── Scalar model forward functions ──────────────────────────
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

Y5 <- function(x, a, b, c, d, g) {
  d + (a - d) / (1 + exp((x - c) / b))^g
}
dydxY5 <- function(x, a, b, c, d, g) {
  u <- exp((x - c) / b)
  -g * (a - d) * u / (b * (1 + u)^(g + 1))
}
d2xY5 <- function(x, a, b, c, d, g, h = 1e-5) {
  (Y5(x + h, a, b, c, d, g) - 2 * Y5(x, a, b, c, d, g) + Y5(x - h, a, b, c, d, g)) / h^2
}

Yd4 <- function(x, a, b, c, d) {
  a + (d - a) / (1 + (x / c)^b)
}
dydxYd4 <- function(x, a, b, c, d) {
  r <- (x / c)^b
  -b * (d - a) * r / (x * (1 + r)^2)
}
d2xYd4 <- function(x, a, b, c, d, h = 1e-5) {
  (Yd4(x + h, a, b, c, d) - 2 * Yd4(x, a, b, c, d) + Yd4(x - h, a, b, c, d)) / h^2
}

Yd5 <- function(x, a, b, c, d, g) {
  a + (d - a) * (1 + g * exp(-b * (x - c)))^(-1 / g)
}
dydxYd5 <- function(x, a, b, c, d, g) {
  u <- 1 + g * exp(-b * (x - c))
  b * (d - a) * exp(-b * (x - c)) * u^(-1 / g - 1)
}
d2xYd5 <- function(x, a, b, c, d, g, h = 1e-5) {
  (Yd5(x + h, a, b, c, d, g) - 2 * Yd5(x, a, b, c, d, g) + Yd5(x - h, a, b, c, d, g)) / h^2
}

Ygomp4 <- function(x, a, b, c, d) {
  a + (d - a) * exp(-exp(-b * (x - c)))
}
dydxYgomp4 <- function(x, a, b, c, d) {
  u <- exp(-b * (x - c))
  b * (d - a) * u * exp(-u)
}
d2xYgomp4 <- function(x, a, b, c, d) {
  e2 <- exp(-(b * (x - c)))
  b^2 * (d - a) * e2 * (e2 - 1) * exp(-e2)
}

# ── Inverse functions ────────────────────────────────────────
inv_Y4 <- function(y, a, b, c, d, tol = 1e-6) {
  y_min <- min(a, d) + tol
  y_max <- max(a, d) - tol
  result <- rep(NA_real_, length(y))
  valid  <- !is.na(y) & y > y_min & y < y_max
  if (any(valid))
    result[valid] <- c + b * log((a - d) / (y[valid] - d) - 1)
  result
}
inv_Y4_fixed <- function(y, fixed_a, b, c, d) {
  c + b * log((fixed_a - d) / (y - d) - 1)
}

inv_Yd4 <- function(y, a, b, c, d) {
  c * (((d - a) / (y - a)) - 1)^(1 / b)
}
inv_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  c * (((d - fixed_a) / (y - fixed_a)) - 1)^(1 / b)
}

inv_Ygomp4 <- function(y, a, b, c, d) {
  c - (1 / b) * log(-log((y - a) / (d - a)))
}
inv_Ygomp4_fixed <- function(y, fixed_a, b, c, d) {
  c - (1 / b) * log(-log((y - fixed_a) / (d - fixed_a)))
}

inv_Y5 <- function(y, a, b, c, d, g) {
  c + b * log(((a - d) / (y - d))^(1 / g) - 1)
}
inv_Y5_fixed <- function(y, fixed_a, b, c, d, g) {
  c + b * log(((fixed_a - d) / (y - d))^(1 / g) - 1)
}

inv_Yd5 <- function(y, a, b, c, d, g) {
  c - (1 / b) * (log(((y - a) / (d - a))^(-g) - 1) - log(g))
}
inv_Yd5_fixed <- function(y, fixed_a, b, c, d, g) {
  c - (1 / b) * (log(((y - fixed_a) / (d - fixed_a))^(-g) - 1) - log(g))
}

# ── dx/dy helpers (used in propagate_error_analytic) ────────
dxdyiY4    <- function(y, a, b, c, d) {
  ratio <- (a - d) / (y - d); b * ratio / ((ratio - 1) * (y - d))
}
dxdyiYd4   <- function(y, a, b, c, d) {
  inner <- ((d - a) / (y - a)) - 1
  -c * (1 / b) * inner^(1 / b - 1) * (d - a) / (y - a)^2
}
dxdyiYgomp4 <- function(y, a, b, c, d) {
  ratio <- (y - a) / (d - a); (1 / b) / ((y - a) * log(ratio))
}
dxdyiY5    <- function(y, a, b, c, d, g) {
  ratio <- (a - d) / (y - d)
  b * (1 / g) * ratio^(1 / g) / ((ratio^(1 / g) - 1) * (y - d))
}
dxdyiYd5   <- function(y, a, b, c, d, g) {
  ratio <- (y - a) / (d - a)
  (1 / b) * g * ratio^(-g) / ((ratio^(-g) - 1) * (y - a))
}

# ── Full gradient functions (free a) — return ALL partials ───
#    grad_theta is a named vector over ALL free parameters.
#    grad_y is the scalar ∂x/∂y.

grad_Y4 <- function(y, a, b, c, d) {
  A  <- (a - d) / (y - d) - 1
  da <-  b / (A * (y - d))
  db <-  log(A)
  dc <-  1
  dd <-  b * (a - y) / (A * (y - d)^2)
  dy <- -b * (a - d) / (A * (y - d)^2)
  list(grad_theta = c(a = da, b = db, c = dc, d = dd), grad_y = dy)
}

grad_Yd4 <- function(y, a, b, c, d) {
  Q  <- ((d - a) / (y - a)) - 1
  p  <- 1 / b
  x  <- c * Q^p
  da <-  (c / b) * Q^(p - 1) * (d - y) / (y - a)^2
  db <- -x * log(Q) / b^2
  dc <-  Q^p
  dd <-  (c / b) * Q^(p - 1) / (y - a)
  dy <- -(c / b) * Q^(p - 1) * (d - a) / (y - a)^2
  list(grad_theta = c(a = da, b = db, c = dc, d = dd), grad_y = dy)
}

grad_Ygomp4 <- function(y, a, b, c, d) {
  R  <- (y - a) / (d - a)
  B  <- -log(R)
  da <-  1 / (b * B * (y - a))
  db <-  log(B) / b^2
  dc <-  1
  dd <- -1 / (b * B * (d - a))
  dy <-  1 / (b * B * (y - a))
  list(grad_theta = c(a = da, b = db, c = dc, d = dd), grad_y = dy)
}

grad_Y5 <- function(y, a, b, c, d, g) {
  H  <- ((a - d) / (y - d))^(1 / g) - 1
  da <-  b / (g * H * (a - d))
  db <-  log(H)
  dc <-  1
  dd <-  b * (-1 / (g * H * (y - d)) - (a - d) / ((y - d)^2 * H))
  dg <- -b * log((a - d) / (y - d)) / (g^2 * H)
  dy <-  b * (a - d) / (g * H * (y - d)^2)
  list(grad_theta = c(a = da, b = db, c = dc, d = dd, g = dg), grad_y = dy)
}

grad_Yd5 <- function(y, a, b, c, d, g) {
  Q  <- ((y - a) / (d - a))^(-g) - 1
  da <-  (1 / (b * Q * g)) * ((y - a) / (d - a))^(-g - 1) * (1 / (d - a))
  db <-  (log(g) - log(Q)) / b^2
  dc <-  1
  dd <-  (1 / (b * Q * g)) * ((y - a) / (d - a))^(-g - 1) * (y - a) / (d - a)^2
  dg <-  (1 / (b * g^2)) * (1 - log(g * Q))
  dy <-  ((y - a) / (d - a))^(-g - 1) / (b * g * Q * (d - a))
  list(grad_theta = c(a = da, b = db, c = dc, d = dd, g = dg), grad_y = dy)
}

# ── Fixed-a gradient functions (only b,c,d partials) ────────
#    Used only when fixed_a is a true external constraint (not from coef).

grad_inv_Y4_fixed <- function(y, fixed_a, b, c, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  inside <- (fixed_a - d) / (y - d) - 1
  db <- log(inside)
  dc <- 1.0
  dd <- (b / (fixed_a - y)) * (1 / (fixed_a - d) - 1 / (y - d))
  c(b = db, c = dc, d = dd)
}

grad_inv_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  inside <- (d - fixed_a) / (y - fixed_a) - 1
  Z  <- inside^(1 / b)
  db <- -c * Z * log(inside) / (b^2)
  dc <- Z
  dd <- (c / b) * Z / ((d - fixed_a) * inside)
  c(b = db, c = dc, d = dd)
}

grad_inv_Ygomp4_fixed <- function(y, fixed_a, b, c, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  L  <- -log((y - fixed_a) / (d - fixed_a))
  db <-  log(L) / (b^2)
  dc <-  1.0
  dd <- -1.0 / (b * L * (d - fixed_a))
  c(b = db, c = dc, d = dd)
}

grad_inv_Y5_fixed <- function(y, fixed_a, b, c, d, g) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c)
  d <- as.numeric(d); g <- as.numeric(g)
  ratio   <- (fixed_a - d) / (y - d)
  ratio_g <- ratio^(1 / g)
  U  <- ratio_g - 1
  db <- log(U)
  dc <- 1.0
  dd <- (b / (g * U)) * ratio_g * (1 / (y - d) - 1 / (fixed_a - d))
  dg <- -(b / g^2) * log(ratio) * ratio_g / U
  c(b = db, c = dc, d = dd, g = dg)
}

grad_inv_Yd5_fixed <- function(y, fixed_a, b, c, d, g) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c)
  d <- as.numeric(d); g <- as.numeric(g)
  ratio   <- (y - fixed_a) / (d - fixed_a)
  ratio_g <- ratio^(-g)
  V  <- ratio_g - 1
  db <- (log(V) - log(g)) / (b^2)
  dc <- 1.0
  dd <- -(1 / b) * (ratio_g / V) * (g / (d - fixed_a))
  dg <- (1 / b) * ((ratio_g / V) * log(ratio) - 1 / g)
  c(b = db, c = dc, d = dd, g = dg)
}

# ── Fixed-a grad_y functions ─────────────────────────────────
grad_y_Y4_fixed <- function(y, fixed_a, b, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); d <- as.numeric(d)
  inside <- (fixed_a - d) / (y - d) - 1
  -b * (fixed_a - d) / ((y - d)^2 * inside)
}

grad_y_Yd4_fixed <- function(y, fixed_a, b, c, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  inside <- (d - fixed_a) / (y - fixed_a) - 1
  -c / b * inside^(1 / b - 1) * (d - fixed_a) / (y - fixed_a)^2
}

grad_y_Ygomp4_fixed <- function(y, fixed_a, b, d) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); d <- as.numeric(d)
  ratio <- (y - fixed_a) / (d - fixed_a)
  L <- -log(ratio)
  1.0 / (b * L * (y - fixed_a))
}

grad_y_Y5_fixed <- function(y, fixed_a, b, d, g) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); d <- as.numeric(d); g <- as.numeric(g)
  ratio   <- (fixed_a - d) / (y - d)
  ratio_g <- ratio^(1 / g)
  U <- ratio_g - 1
  b * (fixed_a - d) / (g * U * (y - d)^2)
}

grad_y_Yd5_fixed <- function(y, fixed_a, b, d, g) {
  y <- as.numeric(y); fixed_a <- as.numeric(fixed_a)
  b <- as.numeric(b); d <- as.numeric(d); g <- as.numeric(g)
  ratio   <- (y - fixed_a) / (d - fixed_a)
  ratio_g <- ratio^(-g)
  V <- ratio_g - 1
  g / (b * (d - fixed_a)) * ratio^(-g - 1) / V
}

# ═══════════════════════════════════════════════════════════════
#  make_inv_and_grad_fixed
#
#  KEY DESIGN:
#    fixed_a is a TRUE external scalar  → use _fixed functions
#                                          grad returns names b,c,d[,g]
#    fixed_a is NULL                    → 'a' is free in coef(fit)
#                                          use full grad_* functions
#                                          grad returns names a,b,c,d[,g]
#
#  The caller (propagate_error_dataframe) must pass
#    fixed_a = NULL  when 'a' is free,
#    fixed_a = <scalar>  when 'a' is a true fixed constant.
# ═══════════════════════════════════════════════════════════════
make_inv_and_grad_fixed <- function(model, y, fixed_a) {

  # Strip any accidental names from scalar inputs
  y <- as.numeric(y)

  # ── Branch A: 'a' is a TRUE external constant ──────────────
  if (!is.null(fixed_a)) {
    fixed_a <- as.numeric(fixed_a)   # strip "a" name if from params["a"]

    return(switch(model,
                  Y4 = list(
                    inv    = function(p) inv_Y4_fixed(y, fixed_a,
                                                      as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad   = function(p) grad_inv_Y4_fixed(y, fixed_a,
                                                           as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad_y = function(p) grad_y_Y4_fixed(y, fixed_a,
                                                         as.numeric(p["b"]), as.numeric(p["d"]))
                  ),
                  Yd4 = list(
                    inv    = function(p) inv_Yd4_fixed(y, fixed_a,
                                                       as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad   = function(p) grad_inv_Yd4_fixed(y, fixed_a,
                                                            as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad_y = function(p) grad_y_Yd4_fixed(y, fixed_a,
                                                          as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"]))
                  ),
                  Ygomp4 = list(
                    inv    = function(p) inv_Ygomp4_fixed(y, fixed_a,
                                                          as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad   = function(p) grad_inv_Ygomp4_fixed(y, fixed_a,
                                                               as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"])),
                    grad_y = function(p) grad_y_Ygomp4_fixed(y, fixed_a,
                                                             as.numeric(p["b"]), as.numeric(p["d"]))
                  ),
                  Y5 = list(
                    inv    = function(p) inv_Y5_fixed(y, fixed_a,
                                                      as.numeric(p["b"]), as.numeric(p["c"]),
                                                      as.numeric(p["d"]), as.numeric(p["g"])),
                    grad   = function(p) grad_inv_Y5_fixed(y, fixed_a,
                                                           as.numeric(p["b"]), as.numeric(p["c"]),
                                                           as.numeric(p["d"]), as.numeric(p["g"])),
                    grad_y = function(p) grad_y_Y5_fixed(y, fixed_a,
                                                         as.numeric(p["b"]), as.numeric(p["d"]), as.numeric(p["g"]))
                  ),
                  Yd5 = list(
                    inv    = function(p) inv_Yd5_fixed(y, fixed_a,
                                                       as.numeric(p["b"]), as.numeric(p["c"]),
                                                       as.numeric(p["d"]), as.numeric(p["g"])),
                    grad   = function(p) grad_inv_Yd5_fixed(y, fixed_a,
                                                            as.numeric(p["b"]), as.numeric(p["c"]),
                                                            as.numeric(p["d"]), as.numeric(p["g"])),
                    grad_y = function(p) grad_y_Yd5_fixed(y, fixed_a,
                                                          as.numeric(p["b"]), as.numeric(p["d"]), as.numeric(p["g"]))
                  ),
                  stop("Unsupported model: ", model)
    ))
  }

  # ── Branch B: 'a' is FREE — must be in p (coef(fit)) ───────
  # Use the full grad_* functions that return ∂x/∂a as well.
  # inv_* free-a versions use the same formulas as inv_*_fixed
  # but read 'a' from p["a"].

  switch(model,
         Y4 = list(
           inv    = function(p) {
             a <- as.numeric(p["a"])
             inv_Y4_fixed(y, a, as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"]))
           },
           grad   = function(p) {
             grads <- grad_Y4(y,
                              a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                              c = as.numeric(p["c"]), d = as.numeric(p["d"]))
             grads$grad_theta   # named c(a=, b=, c=, d=)
           },
           grad_y = function(p) {
             grad_Y4(y,
                     a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                     c = as.numeric(p["c"]), d = as.numeric(p["d"]))$grad_y
           }
         ),
         Yd4 = list(
           inv    = function(p) {
             a <- as.numeric(p["a"])
             inv_Yd4_fixed(y, a, as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"]))
           },
           grad   = function(p) {
             grads <- grad_Yd4(y,
                               a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                               c = as.numeric(p["c"]), d = as.numeric(p["d"]))
             grads$grad_theta
           },
           grad_y = function(p) {
             grad_Yd4(y,
                      a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                      c = as.numeric(p["c"]), d = as.numeric(p["d"]))$grad_y
           }
         ),
         Ygomp4 = list(
           inv    = function(p) {
             a <- as.numeric(p["a"])
             inv_Ygomp4_fixed(y, a, as.numeric(p["b"]), as.numeric(p["c"]), as.numeric(p["d"]))
           },
           grad   = function(p) {
             grads <- grad_Ygomp4(y,
                                  a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                                  c = as.numeric(p["c"]), d = as.numeric(p["d"]))
             grads$grad_theta
           },
           grad_y = function(p) {
             grad_Ygomp4(y,
                         a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                         c = as.numeric(p["c"]), d = as.numeric(p["d"]))$grad_y
           }
         ),
         Y5 = list(
           inv    = function(p) {
             a <- as.numeric(p["a"])
             inv_Y5_fixed(y, a, as.numeric(p["b"]), as.numeric(p["c"]),
                          as.numeric(p["d"]), as.numeric(p["g"]))
           },
           grad   = function(p) {
             grads <- grad_Y5(y,
                              a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                              c = as.numeric(p["c"]), d = as.numeric(p["d"]),
                              g = as.numeric(p["g"]))
             grads$grad_theta
           },
           grad_y = function(p) {
             grad_Y5(y,
                     a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                     c = as.numeric(p["c"]), d = as.numeric(p["d"]),
                     g = as.numeric(p["g"]))$grad_y
           }
         ),
         Yd5 = list(
           inv    = function(p) {
             a <- as.numeric(p["a"])
             inv_Yd5_fixed(y, a, as.numeric(p["b"]), as.numeric(p["c"]),
                           as.numeric(p["d"]), as.numeric(p["g"]))
           },
           grad   = function(p) {
             grads <- grad_Yd5(y,
                               a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                               c = as.numeric(p["c"]), d = as.numeric(p["d"]),
                               g = as.numeric(p["g"]))
             grads$grad_theta
           },
           grad_y = function(p) {
             grad_Yd5(y,
                      a = as.numeric(p["a"]), b = as.numeric(p["b"]),
                      c = as.numeric(p["c"]), d = as.numeric(p["d"]),
                      g = as.numeric(p["g"]))$grad_y
           }
         ),
         stop("Unsupported model: ", model)
  )
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
