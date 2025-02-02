training = read.csv("vowel_training.csv")
testing = read.csv("vowel_testing.csv")

Y = factor(training$y)
X = model.matrix(y ~ 0 + ., data=training)
p = dim(X)[2]
n = length(Y)
classes = levels(Y)
K = length(classes)

nK = matrix(0, nrow = K)
qK = matrix(0, nrow = K)

# Get number of observations in each class
# and estimate proportions
for(k in 1:K){
  nK[k] = sum(Y == classes[k])
  qK[k] = nK[k] / n
}

# Estimate a mean vector muHat_k for each class
muHat = matrix(NA, ncol = p, nrow = K)
for(k in 1:K){
  muHat[k,] = apply(X[Y == classes[k],], 2, mean)
}

SigmaHatList <- list()  # Store covariances per class
for(k in 1:K){
  # Get data for class k
  X_k <- X[Y == classes[k], ]
  n_k <- nK[k]
  
  # Compute class-specific covariance
  muMatrix <- matrix(rep(muHat[k,], n_k), 
                     nrow = n_k, ncol = p, 
                     byrow = TRUE)
  Sigma_k <- t(X_k - muMatrix) %*% (X_k - muMatrix) / (n_k - 1)  # Note: (n_k - 1) for unbiased estimate
  SigmaHatList[[k]] <- Sigma_k  # Assign to list
}

# Compute discriminants-----------------------------------------------
delta = matrix(NA, nrow = n, ncol = K)
for(i in 1:n){
  for(k in 1:K){
    delta[i, k] = qK[k]/sqrt(det(SigmaHatList[[k]])) * exp(-1/2*t(X[i,]-muHat[k,]) %*% solve(SigmaHatList[[k]]) %*% (X[i,]-muHat[k,]))
  }                                                     
}
# --------------------------------------------------------------------------
# Predict observation 1
classes[which.max(delta[1,])]

# Predict all observation
yHat = apply(delta, 1, which.max)

# Compute training accuracy
mean(yHat == Y)


### Testing Accuracy

# Load testing data
testing = read.csv('vowel_testing.csv')
Y_testing = factor(testing$y)
X_testing = model.matrix(y ~ 0 + ., data=testing)

# Compute discriminant for each observation / class
n_testing = length(Y_testing)
delta_testing = matrix(NA, nrow = n_testing, ncol = K)
for(i in 1:n_testing){
  for(k in 1:K){
    delta_testing[i, k] = qK[k] * exp(X_testing[i,] %*% solve(SigmaHatList[[k]]) %*% muHat[k,] - (1/2)*muHat[k,]%*%solve(SigmaHatList[[k]])%*%muHat[k,])
  }
}

# Find class that maximizes discriminant
yHat_testing = classes[apply(delta_testing, 1, which.max)]

# Compute testing accuracy
mean(yHat_testing == Y_testing)


### Sigma Hat exploration - alternate computation options
k = 1
muMatrix = matrix(rep(muHat[k,], nK[k]), 
                  nrow = nK[k], ncol = p,
                  byrow = TRUE)
t(X[Y == classes[k]] - muMatrix) %*% (X[Y == classes[k]] - muMatrix)


Sigma_test = matrix(0, nrow = p, ncol = p)
Xk = X[Y == classes[k],]
for(i in 1:nK[k]){
  Sigma_test = Sigma_test + matrix(Xk[i,] - muHat[k,], nrow = p) %*% matrix(Xk[i,] - muHat[k,], ncol = p)
  
}
Sigma_test

