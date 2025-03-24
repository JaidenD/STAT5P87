set.seed(123)

# Create evenly spaced grid between 0 and 1 for training data
n_train = 100
x_train = seq(0, 1, length.out = n_train)

# Simulate training Y based on the given model:
# Y = 3X^3 - 5X^2 + X - 3 + N(0, 1)
true_function = function(x) {
  3 * x^3 - 5 * x^2 + x - 3
}

# Add noise N(0,1)
y_train = true_function(x_train) + rnorm(n_train, 0, 1)

# Create dense grid for plotting
x_grid = seq(0, 1, length.out = 1000)
y_true = true_function(x_grid)

# Define knots for natural spline and boundaries
ns_knots = c(0.1, 0.26, 0.42, 0.58, 0.74, 0.9)
n_knots = length(ns_knots)
knot_min = min(ns_knots)
knot_max = max(ns_knots)

# Helper function for truncated cubic power
d_function = function(x, knot) {
  pmax(0, (x - knot))^3
}

# Function to run simulation multiple times and compute variance
run_simulation = function(n_sims = 100) {
  # Initialize matrices to store predictions
  linear_preds = matrix(0, nrow = n_sims, ncol = length(x_grid))
  cubic_preds = matrix(0, nrow = n_sims, ncol = length(x_grid))
  cubic_spline_preds = matrix(0, nrow = n_sims, ncol = length(x_grid))
  natural_spline_preds = matrix(0, nrow = n_sims, ncol = length(x_grid))
  
  # Run simulations
  for (i in 1:n_sims) {
    # Generate new training data with same X but different noise
    y_sim = true_function(x_train) + rnorm(n_train, 0, 1)
    
    # Linear model
    linear_model = lm(y_sim ~ x_train)
    linear_preds[i,] = predict(linear_model, newdata = data.frame(x_train = x_grid))
    
    # Cubic
    cubic_model = lm(y_sim ~ poly(x_train, 3, raw = TRUE))
    cubic_preds[i,] = predict(cubic_model, newdata = data.frame(x_train = x_grid))
    
    # Cubic spline with knots at 1/3 and 2/3
    x_basis = cbind(1, x_train, x_train^2, x_train^3, 
                    pmax(0, (x_train - 1/3))^3, 
                    pmax(0, (x_train - 2/3))^3)
    cubic_spline_model = lm(y_sim ~ x_basis - 1)
    x_pred_basis = cbind(1, x_grid, x_grid^2, x_grid^3, 
                         pmax(0, (x_grid - 1/3))^3, 
                         pmax(0, (x_grid - 2/3))^3)
    cubic_spline_preds[i,] = x_pred_basis %*% cubic_spline_model$coefficients
    
    # Natural spline basis for training data
    ns_basis_train = matrix(0, nrow = length(x_train), ncol = n_knots)
    for (j in 1:n_knots) {
      alpha_j = (knot_max - ns_knots[j]) / (knot_max - knot_min)
      beta_j = (ns_knots[j] - knot_min) / (knot_max - knot_min)
      ns_basis_train[, j] = d_function(x_train, ns_knots[j]) -
        alpha_j * d_function(x_train, knot_max) -
        beta_j * d_function(x_train, knot_min)
    }
    colnames(ns_basis_train) = paste0("ns_basis", 1:n_knots)
    ns_train_df = data.frame(x_train = x_train, ns_basis_train)
    
    # Fit natural spline model
    ns_model = lm(y_sim ~ ., data = ns_train_df)
    
    # Natural spline basis for prediction
    ns_basis_pred = matrix(0, nrow = length(x_grid), ncol = n_knots)
    for (j in 1:n_knots) {
      alpha_j = (knot_max - ns_knots[j]) / (knot_max - knot_min)
      beta_j = (ns_knots[j] - knot_min) / (knot_max - knot_min)
      ns_basis_pred[, j] = d_function(x_grid, ns_knots[j]) -
        alpha_j * d_function(x_grid, knot_max) -
        beta_j * d_function(x_grid, knot_min)
    }
    colnames(ns_basis_pred) = paste0("ns_basis", 1:n_knots)
    ns_pred_df = data.frame(x_train = x_grid, ns_basis_pred)
    
    # Predict
    natural_spline_preds[i,] = predict(ns_model, newdata = ns_pred_df)
  }
  
  # Compute pointwise variances
  linear_var = apply(linear_preds, 2, var)
  cubic_var = apply(cubic_preds, 2, var)
  cubic_spline_var = apply(cubic_spline_preds, 2, var)
  natural_spline_var = apply(natural_spline_preds, 2, var)
  
  return(list(
    x = x_grid,
    linear_var = linear_var,
    cubic_var = cubic_var,
    cubic_spline_var = cubic_spline_var,
    natural_spline_var = natural_spline_var
  ))
}

# Run the simulation
results = run_simulation(n_sims = 100)

# Create plot
max_y = max(c(results$cubic_spline_var, results$natural_spline_var), na.rm = TRUE)

plot(results$x, results$linear_var, type = "l", col = "orange", lwd = 2,
     xlab = "X", ylab = "Pointwise Variances",
     ylim = c(0, max_y * 1.1),
     main = "Pointwise Variance of Different Spline Models")
lines(results$x, results$cubic_var, col = "red", lwd = 2)
lines(results$x, results$cubic_spline_var, col = "green", lwd = 2)
lines(results$x, results$natural_spline_var, col = "blue", lwd = 2)

# Add legend
legend("topleft", 
       legend = c("Linear", "Cubic", "Cubic Spline", "Natural Spline"),
       col = c("orange", "red", "green", "blue"),
       lty = c(1, 2, 1, 4), 
       lwd = 2)
