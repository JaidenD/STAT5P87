SMO = function(x, y, C = 1, threshold = 1e-3, max_iter = 1000) {
  # x: n x d matrix (n samples, d features)
  # y: length-n vector of labels (+1 or -1)
  # C: SVM hyperparameter (soft margin constant)
  # threshold: tolerance for change in alpha
  # max_iter: maximum number of iterations
  
  n = nrow(x)
  d = ncol(x)
  
  # Initialize Lagrange multipliers and weight vector
  alpha = rep(0, n)      # Lagrange multipliers for each sample
  B = rep(0, d)          # Weight vector (d features)
  b0 = 0                 # Bias term
  iter = 0
  
  # Error function: f(x_i) = x[i,] %*% B - b0, so error = f(x_i) - y[i]
  E = function(i) {
    as.numeric(x[i, ] %*% B) - b0 - y[i]
  }
  
  # Main SMO loop: iterate until convergence or maximum iterations reached.
  while (iter < max_iter) {
    iter = iter + 1
    alpha_prev = alpha
    
    # Loop over all pairs (i, j)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j) next
        
        # Compute current errors for sample i and j (using the most recent B and b0)
        E_i = E(i)
        E_j = E(j)
        
        # Compute eta = K(x_i, x_i) + K(x_j, x_j) - 2K(x_i, x_j)
        # For linear kernel, K(u,v)= sum(u*v)
        eta = sum(x[i, ] * x[i, ]) + sum(x[j, ] * x[j, ]) - 2 * sum(x[i, ] * x[j, ])
        if (abs(eta) < 1e-12 || is.na(eta)) next  # avoid division by zero or invalid updates
        
        # Store old alpha values for i and j
        alpha_i_old = alpha[i]
        alpha_j_old = alpha[j]
        
        # Compute tentative new alpha_j (unclipped)
        alpha_j_new = alpha_j_old - y[j] * (E_i - E_j) / eta
        
        # Compute bounds L and H for alpha_j based on whether y[i] equals y[j]
        if (y[i] != y[j]) {
          L = max(0, alpha_j_old - alpha_i_old)
          H = min(C, C + alpha_j_old - alpha_i_old)
        } else {
          L = max(0, alpha_j_old + alpha_i_old - C)
          H = min(C, alpha_j_old + alpha_i_old)
        }
        
        # If L equals H or if update is not feasible, skip to next pair.
       # if (L == H || is.na(L) || is.na(H) || is.na(alpha_j_new)) next
        
        # Clip new alpha_j into [L, H]
        if (alpha_j_new > H) {
          alpha_j_new = H
        } else if (alpha_j_new < L) {
          alpha_j_new = L
        }
        
        # If the change in alpha_j is very small, skip updating this pair.
        if (abs(alpha_j_new - alpha_j_old) < 1e-5) next
        
        # Compute new alpha_i based on the constraint
        alpha_i_new = alpha_i_old + y[i]*y[j]*(alpha_j_old - alpha_j_new)
        
        # (No need to clip alpha_i_new explicitly if L and H are correctly computed.)
        # Update alphas
        alpha[i] = alpha_i_new
        alpha[j] = alpha_j_new
        
        # Incrementally update the weight vector B.
        # The change in B due to alpha_i and alpha_j updates:
        delta_i = alpha[i] - alpha_i_old
        delta_j = alpha[j] - alpha_j_old
        B = B + delta_i * y[i] * x[i, ] + delta_j * y[j] * x[j, ]
      }
    }
    
    # Recompute bias b0 using support vectors (those with 0 < alpha < C).
    S = which(alpha > 1e-12 & alpha < (C - 1e-12))
    if (length(S) > 0) {
      # For support vector i, ideally we have: x[i,] %*% B - b0 = 1/y[i]
      # So, b0 = x[i,] %*% B - 1/y[i].
      b0_values = sapply(S, function(i) {
        as.numeric(x[i, ] %*% B) - 1 / y[i]
      })
      b0 = mean(b0_values)
    } else {
      b0 = 0
    }
    
    # Check convergence: if the maximum change in alphas is below the threshold, break.
    if (max(abs(alpha - alpha_prev)) < threshold) break
  }
  
  return(list(alpha = alpha, B = B, b0 = b0, iterations = iter))
}

# Example usage:
# Assume "separable-data.csv" has features in the first two columns and labels in the third column.
data = read.csv("separable-data.csv")
# x is n x d (each row a sample, each column a feature)
X = as.matrix(data[, 1:2])
y = data[, 3]

# Fit the SVM using the SMO algorithm
res = SMO(X, y, C = 1, threshold = 1e-3, max_iter = 1000)

alpha = res$alpha
B = res$B
b0 = res$b0

# Plot the data points and decision boundary.
plot(
  X[, 1], X[, 2],
  col = ifelse(y == 1, "red", "blue"),
  pch = 19,
  xlab = "x1", ylab = "x2",
  main = "Linear SVM Decision Boundary"
)

# The decision boundary is defined by: B[1]*x1 + B[2]*x2 - b0 = 0.
if (abs(B[2]) > 1e-12) {
  abline(a = b0 / B[2], b = -B[1] / B[2], lwd = 2)
} else {
  abline(v = b0 / B[1], lwd = 2)
}

cat("Estimated alpha values:\n")
print(alpha)
cat("\nIterations: ", res$iterations, "\n")
