# Part a

mixture <- function(mu=c(mu1, mu2), sigma= c(sigma1,sigma2), p, n){
  observations = vector()
  
  for(i in 1:n){
    if(rbinom(n = 1, size = 1, prob = p) == 1){observations[i] = rnorm(1, mu[1], sigma[1])}
    else{observations[i] = rnorm(1, mu[2], sigma[2])}
  }
  return(observations)
}
# Define the mixtures

p0 <- function(x) {
  # Mixture for y=0
  0.5*dnorm(x, mean=0.2, sd=sqrt(0.04)) + 0.5* dnorm(x, mean=0.6, sd=sqrt(0.09))
}

p1 <- function(x) {
  # Mixture for y=1
  0.5*dnorm(x, mean=0.5, sd=sqrt(0.04)) + 0.5* dnorm(x, mean=0.8, sd=sqrt(0.01))
}

f <- function(x) {
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

simulate_data <- function(n) {
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

lda_classifier <- function(training, testing) {
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

bayes_classifier <- function(x){
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
