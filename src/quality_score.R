# Parameters (example values)
a <- 100   # Min asymptote
d <- 1000  # Max asymptote
c <- 125    # Inflection point
b <- 2.5   # Slope factor
g <- 1   # Asymmetry factor

# 5PL function
Y_5PL <- function(X, a, d, c, b, g) {
  d + (a - d) / (1 + (X / c)^b)^g
}

y_5pl <- function(x, a, d, c, b, g) {
  d + (a - d) / (1 + (x / c)^b)^g
}

# Derivative dY/dX
dYdX <- function(X, a, d, c, b, g) {
  u <- (X / c)^b
  -(a - d) * g * (b / c) * (X / c)^(b - 1) * (1 + u)^(-g - 1)
}

# Inverse function: X as a function of Y
X_5PL_inv <- function(Y, a, d, c, b, g) {
  term <- ((a - d) / (Y - d))^(1 / g) - 1
  c * term^(1 / b)
}

# Derivative dX/dY
dXdY <- function(Y, a, d, c, b, g) {
  Q <- ((a - d)/(Y - d))^(1 / g) - 1
  dQdY <- -((a - d) / (g * (Y - d)^2)) * ((a - d)/(Y - d))^((1 - g)/g)

  c * (1/b) * Q^(1/b - 1) * dQdY
}

# Plotting
library(ggplot2)
library(tidyr)
library(dplyr)

# X range for functions of X
X_vals <- seq(1, 300, length.out = 500)
Y_vals <- Y_5PL(X_vals, a, d, c, b, g)
dY_vals <- dYdX(X_vals, a, d, c, b, g)

# Y range for inverse functions (must be within bounds)
Y_min <- min(Y_vals[Y_vals != d]) + 0.1
Y_max <- max(Y_vals[Y_vals != a]) - 0.1
Y_vals_inv <- seq(Y_min, Y_max, length.out = 500)
X_inv_vals <- X_5PL_inv(Y_vals_inv, a, d, c, b, g)
dX_vals <- dXdY(Y_vals_inv, a, d, c, b, g)

# Plot Y(X) and dY/dX
df1 <- data.frame(
  X = X_vals,
  Y = Y_vals,
  dYdX = dY_vals
) %>% pivot_longer(cols = c("Y", "dYdX"), names_to = "Function", values_to = "Value")

p1 <- ggplot(df1, aes(x = X, y = Value)) +
  geom_line(size=1) +
  labs(title = "5PL: Y(X) and dY/dX", x = "X (Conc)", y = "Value") +
  facet_wrap(~ Function, scales = "free") +
  theme_minimal()

# Plot X(Y) and dX/dY
df2 <- data.frame(
  Y = Y_vals_inv,
  X = X_inv_vals,
  dXdY = dX_vals
) %>% pivot_longer(cols = c("X", "dXdY"), names_to = "Function", values_to = "Value")

p2 <- ggplot(df2, aes(x = Y, y = Value)) +
  geom_line(size=1) +
  labs(title = "Inverse 5PL: X(Y) and dX/dY", x = "Y (MFI)", y = "Value") +
  facet_wrap(~ Function, scales = "free") +
  theme_minimal()

# Display plots
print(p1)
print(p2)
