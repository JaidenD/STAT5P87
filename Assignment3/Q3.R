# Part a
kernel_smoothing = function(x0, X, Y, K, h, lambda = 2){
  # Inputs
  #   x0 - input to be predicted
  #   X - matrix of training inputs (n x p)
  #   Y - matrix of training outputs (n x 1)
  #   k - kernel function
  #   h - bandwidth function
  #
  # Outputs
  #   predicted y0 value

  distances = abs(x0 - X)
  bandwidth = h(lambda, x0, X)
  
  # Case 1: bandwidth is zero
  if(bandwidth == 0){
    # Use mean of points that have the same x-value
    return(mean(Y[which(X == x0)]))
  }
  
  # Compute kernel weights
  w = K(distances/bandwidth)
  
  # Case 2: sum of weights is zero
  if(sum(w) == 0){
    # Identify boundary points
    return(mean(Y[which(distances/bandwidth == 1)]))
  }
  
  # Base case
  return(sum(w*Y) / sum(w))
}

# Part b
data = read.csv("concrete-data.csv")
x = data$age
y = data$strength
make_folds = function(Y, nFolds, stratified = FALSE, seed = 0){
  # K-Fold cross validation
  # Input:
  #   Y (either sample size, or vector of outputs)
  #   stratified (boolean): whether the folds should 
  #     be stratified. If TRUE then Y should be a vector of outputs
  # Output: list of vectors of fold indices
  if(stratified & length(Y) == 1){
    stop('For stratified folds, Y must be a vector of outputs')
  }
  n = ifelse(length(Y) > 1, length(Y), Y)
  index = c(1:n)
  if(stratified){
    Y = factor(Y)
    classes = levels(Y)
    nClasses = length(classes)
    if(nClasses == 1){
      stop('stratified requires more than one class')
    }
    classfolds = list()
    for(class in 1:nClasses){
      classfolds[[class]] = list()
      classIndex = index[Y == classes[class]]
      n_class = sum(Y == classes[class])
      n_per_fold = floor(n_class / nFolds)
      shuffled_index = sample(classIndex)
      for(fold in c(1:(nFolds - 1))){
        classfolds[[class]][[fold]] = shuffled_index[c((1 + (fold - 1) * n_per_fold):(fold * n_per_fold))]
      }
      classfolds[[class]][[nFolds]] = shuffled_index[c(((nFolds - 1)*n_per_fold + 1):n_class)]
    }
    folds = list()
    for(fold in 1:nFolds){
      folds[[fold]] = classfolds[[1]][[fold]]
      for(class in 2:nClasses){
        folds[[fold]] = c(folds[[fold]], classfolds[[class]][[fold]])
      }
    }
  }else{
    folds = list()
    n_per_fold = floor(n / nFolds)
    shuffled_index = sample(index)
    for(fold in c(1:(nFolds - 1))){
      folds[[fold]] = shuffled_index[c((1 + (fold - 1) * n_per_fold):(fold * n_per_fold))]
    }  
    folds[[nFolds]] = shuffled_index[c(((nFolds - 1)*n_per_fold + 1):n)]
  }
  return(folds)
}

t = seq(from=-2, to=2, by = 0.01)

gaussian_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Gaussian kernel
  return((2*pi)^(-1/2) * exp(-t^2/2))
}
epanechnikov_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Epanechnikov kernel
  return(as.integer(abs(t) <= 1) * (3/4) * (1 - t^2))
}

adaptive_bandwidth = function(lambda, x0, X){
  # Input:
  #   lambda - positive integer, number of nearest neighbours
  #   x0 - scalar, point where we will compute bandwidth
  #   X - vector of training observations
  #
  # Output: the distance from x0 to its lambda^th nearest neighbour in X
  N = length(X)
  d = matrix(0, nrow = N)
  for(i in c(1:N)){
    d[i] = abs(x0 - X[i])
  } 
  d_sorted = sort(d)
  return(d_sorted[lambda])
}

cv_kernel_smoothing = function(x, y, folds, kernel_fn, bandwidth_fn, lambda){
  mse_per_fold = numeric(length(folds))
  
  for(i in 1:10){
    test_idx  = folds[[i]]
    train_idx = setdiff(seq_along(x), test_idx)
    
    xtrain = x[train_idx]
    ytrain = y[train_idx]
    xtest  = x[test_idx]
    ytest  = y[test_idx]
    
    # Predictions on the test fold
    ypred = sapply(xtest, function(x0) {
      kernel_smoothing(x0, xtrain, ytrain, kernel_fn, bandwidth_fn, lambda)
    })
    
    # Mean squared error for this fold
    mse_per_fold[i] = mean((ytest - ypred)^2)
  }
  return(mean(mse_per_fold))
}

set.seed(1)
foldNum = 10
folds = make_folds(length(y), nFolds = foldNum)

# Find optimal lambda
lambda_candidates = seq(1,29,1)

kernel_list = list(gaussian = gaussian_kernel, epanechnikov = epanechnikov_kernel)
results = list()

# Loop over the kernels
for(kernel_name in names(kernel_list)){
  kernel_fn = kernel_list[[kernel_name]]
  
  best_lambda = NA
  best_mse    = Inf
  
  # Loop over lambda candidates for current kernel
  for(lambda in lambda_candidates) {
    mse_cv = cv_kernel_smoothing(
      x, y, folds,
      kernel_fn,
      adaptive_bandwidth,
      lambda
    )
    if(mse_cv < best_mse) {
      best_mse    = mse_cv
      best_lambda = lambda
    }
  }
  
  results[[kernel_name]] = list(kernel_fn = kernel_fn,
                                best_lambda = best_lambda,
                                best_mse = best_mse)
}
best_kernel_name = names(results)[which.min(sapply(results, function(res) res$best_mse))]
best_lambda_overall = results[[best_kernel_name]]$best_lambda
best_mse_overall    = results[[best_kernel_name]]$best_mse


# Part c

x_grid = seq(min(x), max(x), length.out = 200)

# Compute predicted y-values on grid
y_hat_grid <- sapply(x_grid, function(x0) {
  kernel_smoothing(
    x0,
    x, 
    y,
    epanechnikov_kernel,
    adaptive_bandwidth,
    best_lambda
  )
})

plot(
  x, y,
  pch  = 19,
  col  = "blue",
  xlab = "Age",
  ylab = "Strength",
  main = paste("Epanechnikov Kernel Smoothing with \u03BB = 18")
)
lines(
  x_grid, y_hat_grid,
  col = "red",
  lwd = 2
)
legend(
  "topleft",
  legend = c("Data", "Fitted Curve"),
  pch    = c(19, NA),
  col    = c("blue", "red"),
  lty    = c(NA, 1),
  lwd    = c(NA, 2)
)

