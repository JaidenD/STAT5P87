# Q1 #########################################################################
# mu - mean
# sigma = variance
# p = probability of observing value from first distribution
# n = number of samples
mixture = function(mu=c(mu1, mu2), sigma= c(sigma1,sigma2), p, n){
  observations = vector()
  
  for(i in 1:n){
    if(rbinom(n = 1, size = 1, prob = p) == 1){observations[i] = rnorm(1, mu[1], sigma[1])}
    else{observations[i] = rnorm(1, mu[2], sigma[2])}
  }
  return(observations)
}

# Q2 #########################################################################
createFolds = function(y, k, stratified = FALSE, seed = 0) {
  # Set the random seed
  set.seed(seed)
  
  # If y is a single number, treat it as sample size and create an index vector.
  if (length(y) == 1 && is.numeric(y)) {
    n = y
    if (stratified) {
      warning("Stratification is only valid for categorical outputs. Using unstratified folds.")
      stratified = FALSE
    }
    y = seq_len(n)
  } else {
    n = length(y)
  }
  
  folds = vector("list", k)
  
  if (!stratified) {
    # Unstratified: Simply randomize indices and assign to folds.
    indices = sample(n)
    
    # Compute fold sizes: most folds will have floor(n/k) observations;
    # the first few folds will get an extra one if n is not divisible by k.
    fold_sizes = rep(floor(n/k), k)
    remainder = n - sum(fold_sizes)
    if (remainder > 0) {
      fold_sizes[1:remainder] = fold_sizes[1:remainder] + 1
    }
    
    start_index = 1
    for (i in seq_len(k)) {
      end_index = start_index + fold_sizes[i] - 1
      folds[[i]] = indices[start_index:end_index]
      start_index = end_index + 1
    }
    
  } else {
    # Stratified: The fold structure will preserve class proportions.
    # First, ensure that y is a factor.
    if (!is.factor(y)) {
      y = as.factor(y)
    }
    
    # Initialize each fold as an empty vector.
    for (i in seq_len(k)) {
      folds[[i]] = integer(0)
    }
    
    # For each level (class), split the indices of that class into k roughly equal parts.
    for (lvl in levels(y)) {
      lvl_indices = which(y == lvl)
      lvl_indices = sample(lvl_indices)  # Randomize the indices for this level.
      n_lvl = length(lvl_indices)
      
      # Compute fold sizes for this class.
      fold_sizes = rep(floor(n_lvl/k), k)
      remainder = n_lvl - sum(fold_sizes)
      if (remainder > 0) {
        fold_sizes[1:remainder] = fold_sizes[1:remainder] + 1
      }
      
      start_index = 1
      for (i in seq_len(k)) {
        end_index = start_index + fold_sizes[i] - 1
        folds[[i]] = c(folds[[i]], lvl_indices[start_index:end_index])
        start_index = end_index + 1
      }
    }
  }
  
  # Return the folds for both cases
  return(folds)
}

# Test Case 1: Unstartified
folds_numeric = createFolds(10, 2, stratified = FALSE, seed = 0)
folds_numeric

# Test Case 2: Startified
y_cat = c("A", "A", "B", "B", "A", "B", "A", "B", "B", "A")

folds_stratified = createFolds(y_cat, 2, stratified = TRUE, seed = 0)
folds_stratified

# Q3 #########################################################################
# Part a

mixture = function(mu=c(mu1, mu2), sigma= c(sigma1,sigma2), p, n){
  observations = vector()
  
  for(i in 1:n){
    if(rbinom(n = 1, size = 1, prob = p) == 1){observations[i] = rnorm(1, mu[1], sigma[1])}
    else{observations[i] = rnorm(1, mu[2], sigma[2])}
  }
  return(observations)
}
# Define the mixtures

p0 = function(x) {
  # Mixture for y=0
  0.5*dnorm(x, mean=0.2, sd=sqrt(0.04)) + 0.5* dnorm(x, mean=0.6, sd=sqrt(0.09))
}

p1 = function(x) {
  # Mixture for y=1
  0.5*dnorm(x, mean=0.5, sd=sqrt(0.04)) + 0.5* dnorm(x, mean=0.8, sd=sqrt(0.01))
}

f = function(x) {
  0.4*p0(x) - 0.6*p1(x)
}

# Find the roots numerically
xs = seq(-1, 2, length.out=2000)
vals = f(xs)

# Assuming continuity we will use intermediate value theorem
sign_changes = which(vals[-1]*vals[-length(vals)]<0)

# For each interval where there's a sign change, we apply uniroot to find the root.
boundaries = numeric(0)
for(i in sign_changes) {
  root = uniroot(f, interval=c(xs[i], xs[i+1]))$root
  boundaries = c(boundaries, root)
}

boundaries
# Classify 0.366<x<1.035 as class 1, otherwise class 0

# Part b #########################################################################################

mu = c(0.2, 0.6)
sigma = c(0.04, 0.09)

set.seed(0)
training = mixture(mu, sigma, 0.5, 200)
testing = mixture(mu, sigma, 0.5, 1000)

simulate_data = function(n) {
  y = rbinom(n, size=1, prob=0.6)
  
  x = numeric(n)
  for (i in seq_len(n)) {
    if (y[i] == 0) {
      # Mixture for y=0:
      x[i] = mixture(
        mu    = c(0.2, 0.6), 
        sigma = c(sqrt(0.04), sqrt(0.09)), 
        p     = 0.5, 
        n     = 1
      )
    } else {
      # Mixture for y=1:
      x[i] = mixture(
        mu    = c(0.5, 0.8), 
        sigma = c(sqrt(0.04), sqrt(0.01)), 
        p     = 0.5, 
        n     = 1
      )
    }
  }
  data.frame(x = x, y = factor(y))
}

lda_classifier = function(training, testing) {
  # Extract response and predictor matrix from training data
  Y = factor(training$y)
  X = model.matrix(y ~ 0 + ., data = training)
  
  # Determine classes and dimensions
  classes = levels(Y)
  K = length(classes)
  n = nrow(X)
  p = ncol(X)
  
  nK = table(Y)
  qK = nK/n
  
  # Estimate means
  muHat = matrix(NA, nrow = K, ncol = p)
  for (k in 1:K) {
    muHat[k, ] = colMeans(X[Y == classes[k], , drop = FALSE])
  }
  
  # Compute covariance matrices
  SigmaHatList = list()
  for (k in 1:K) {
    X_k = X[Y == classes[k], , drop = FALSE]
    SigmaHatList[[k]] = cov(X_k)
  }
  
  # Compute the total covariance matrix
  totalSigma = Reduce("+", lapply(1:K, function(k) {
    (nK[k] - 1) * SigmaHatList[[k]]
  })) / (n - K)
  
  # Compute the inverse of the pooled covariance matrix
  invtotalSigma = solve(totalSigma)
  
  Y_testing = factor(testing$y)
  X_testing = model.matrix(y ~ 0 + ., data = testing)
  n_testing = nrow(X_testing)
  
  # Compute LDA discriminant functions for each test observation:
  delta_testing = matrix(NA, nrow = n_testing, ncol = K)
  for (k in 1:K) {
    delta_testing[, k] = X_testing %*% (invtotalSigma %*% muHat[k,]) -
      0.5 * as.numeric(t(muHat[k,]) %*% invtotalSigma %*% muHat[k,]) +
      log(qK[k])
  }
  
  # Assign each test observation to the class with the maximum discriminant value
  yHat_testing = classes[apply(delta_testing, 1, which.max)]
  
  # Compute testing accuracy
  test_acc = mean(yHat_testing == Y_testing)
  
  return(list(
    test_accuracy = test_acc,
    test_predictions = yHat_testing
  ))
}

set.seed(0)

# Generate training and testing data
train_data = simulate_data(200)
test_data  = simulate_data(1000)

# Train and evaluate LDA
lda_results = lda_classifier(train_data, test_data)
lda_results$test_accuracy # 0.721

bayes_classifier = function(x){
  p_y0_given_x = (p0(x) * 0.4) / ((p0(x) * 0.4) + (p1(x) * 0.6))
  return(ifelse(p_y0_given_x > 0.5, "0", "1"))
}

# Run bayes classifier
bayes_preds = sapply(test_data$x, bayes_classifier)
bayes_preds = factor(bayes_preds, levels = c("0", "1"))

# Compute Bayes accuracy
bayes_accuracy = mean(bayes_preds == test_data$y)
bayes_accuracy # 0.729

# The Bayes classifier has better testing accuracy than LDA

# Q4 #########################################################################
library(glmnet)
data = read.csv("a2-vowel-data.csv")

head(data)

X = model.matrix(y ~ x1 +x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data = data) # Input
y = data$y # Output

set.seed(0)

fit = cv.glmnet(X, y, family = "multinomial", alpha = 1, nfolds = 5, type.measure = "class")

lambda = fit$lambda
accuracy = 1 - fit$cvm # accuracy = 1 - misclassifications

best_lambda = fit$lambda.min # Best Lambda: 0.0004371341
max_accuracy = max(accuracy) # Maximum accuracy: 0.6
