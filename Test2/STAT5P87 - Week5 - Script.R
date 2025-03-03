### STAT5P87 - Week 5 ###

# Functions to replicate Week 2 lab

# A function to simulate training input data
# Each input is independent Unif(-1, 1)
simulate_X = function(n, p){
  # Input:
  # 	n is the number of samples/observations
  #   p is the dimension of the inputs
  #
  # Output: 
  # 	X is an n x p matrix of independent Uniform(-1, 1) values 
  #
  X = matrix(2 * runif(n * p) - 1, nrow = n, ncol = p)
  return(X)
}

# A function to simulate training output data
# Based on a model: Y = \sum_{j = 1}^p X_j / j + e
#  where e ~ N(0, sigma^2)
simulate_Y = function(X, B, sigma){
  # Input:
  # 	X: n * p matrix of input values
  #   B: p * 1 vector of coefficients
  #   sigma: standard deviation of e
  #
  # Output: 
  # 	Y: n x 1 column vector of output values 
  #
  n = dim(X)[1]
  E = matrix(rnorm(n, mean = 0, sd = sigma), nrow = n)
  Y = X %*% B + E
  return(Y)
}

### Pooled Covariance function
pooled_cov = function(X, Y, mu){
  # X is the data (N x p, continuous)
  # G is the observed class (N x 1, factor)
  # mu is a mean vector (K x p)
  classes = levels(Y)
  n = dim(X)[1]
  p = dim(X)[2]
  K = dim(mu)[1]
  Sigma = matrix(0, nrow = p, ncol = p)
  for(k in c(1:K)){
    Sigma = Sigma + (t(X[Y == classes[k],]) - mu[k,]) %*% t(t(X[Y == classes[k],]) - mu[k,])
  }
  return(Sigma / (n - K))
}



### Exploring the bias-variance tradeoff for linear regression  

# Set randomizer seed
set.seed(0)

# Input dimension
p = 15

# How many times to iterate the simulation
n_iterations = 1000

# Size of training data
n_training = 30

# True Model: 
#
# b0 = 0, beta_j = 2^(-j)
# X_{i,j} are iid Unif(0, 1)
# Y = XB + E
# where E ~ N(0, sigma^2)

B = matrix(0, nrow = p)
for(j in c(1:p)){
  B[j] = 1/j
}
sigma = 1

# Independent X_new values to be predicted
Xnew = matrix(1/2, ncol = p)

# Expected Y_new value
EYnew = Xnew %*% B

# Initialize results
yHat = matrix(0, nrow = n_iterations, ncol = p)

# Simulate training/testing sets to estimate bias and variance
for(iter in c(1:n_iterations)){
  # Simulate training data
  trainingX = simulate_X(n_training, p)
  trainingY = simulate_Y(trainingX, B, sigma)
  
  for(k in c(1:p)){
    X0 = matrix(1, nrow = n_training, ncol = (k+1))
    X0[,c(2:(k+1))] = trainingX[,c(1:k)]
    
    # least-squares estimate of B
    bHat = solve(t(X0) %*% X0) %*% t(X0) %*% trainingY
    
    # X0_new value for k inputs with intercept
    X0new = c(1, Xnew[1:k])
    
    # Predicted Y_new value
    yHat[iter, k] = X0new %*% bHat
  }
}  

squared_bias = vector('numeric', length = p)
variance = vector('numeric', length = p)

for(k in c(1:p)){
  squared_bias[k] = (mean(yHat[,k]) - EYnew)^2
  variance[k] = var(yHat[,k])
}

mse = variance + squared_bias
y_max = max(mse)

plot(mse, type = 'b', xlab = 'k', ylab = 'units squared', bty = 'n', main = 'Bias-Variance Decomposition of MSE', 
     col = 'purple', pch = 1, lwd = 2, ylim = c(0, y_max), xlim = c(1, 15), cex.lab = 1.25)
lines(squared_bias, type = 'b', col = 'blue', pch = 2, lwd = 1, lty = 2)
lines(variance, type = 'b', col = 'red', lty = 2, lwd = 1, pch = 2)

legend(6, 1.5, c('mean-squared error', 'squared bias', 'variance'), bty = 'n', cex = 1.25, lwd = c(2, 1, 1), lty = c(1, 2, 2), col = c('purple', 'blue', 'red'), pch = c(1, 2, 2))





### Exploring training/testing error for linear regression  

# Set randomizer seed
set.seed(0)

# Input dimension
p = 15

# How many times to iterate the simulation
n_iterations = 1000

# Size of training data
n_training = 50
n_testing = 30

# True Model: 
#
# b0 = 0, beta_j = 2^(-j)
# X_{i,j} are iid Unif(0, 1)
# Y = XB + E
# where E ~ N(0, sigma^2)

B = matrix(0, nrow = p)
for(j in c(1:p)){
  B[j] = 1/j
}
sigma = 1


# Initialize results
training_mse = matrix(0, nrow = n_iterations, ncol = p)
testing_mse = matrix(0, nrow = n_iterations, ncol = p)

# Simulate training/testing sets to estimate bias and variance
for(iter in c(1:n_iterations)){
  # Simulate training data
  trainingX = simulate_X(n_training, p)
  trainingY = simulate_Y(trainingX, B, sigma)
  
  testingX = simulate_X(n_testing, p)
  testingY = simulate_Y(testingX, B, sigma)
  
  for(k in c(1:p)){
    trainingX0 = matrix(1, nrow = n_training, ncol = (k+1))
    trainingX0[,c(2:(k+1))] = trainingX[,c(1:k)]
    
    testingX0 = matrix(1, nrow = n_testing, ncol = (k+1))
    testingX0[,c(2:(k+1))] = testingX[,c(1:k)]
    
    # least-squares estimate of B
    bHat = solve(t(trainingX0) %*% trainingX0) %*% t(trainingX0) %*% trainingY
    
    # Predicted Y values for training/testing
    trainingyHat = trainingX0 %*% bHat
    testingyHat = testingX0 %*% bHat
    
    # Training/testing MSE
    training_mse[iter, k] = sum((trainingY - trainingyHat)^2) / n_training
    testing_mse[iter, k] = sum((testingY - testingyHat)^2) / n_testing
  }
}  

training_mse = apply(training_mse, 2, mean)
testing_mse = apply(testing_mse, 2, mean)

plot(training_mse, type = 'b', xlab = 'model complexity', ylab = 'mse', bty = 'n', main = 'Training/Testing MSE', 
     col = 'purple', pch = 1, lwd = 2, ylim = c(0.5, 1.5), xlim = c(1, 15), cex.lab = 1.25)
lines(testing_mse, type = 'b', col = 'blue', pch = 2, lwd = 2, lty = 2)

legend(2, 0.5, c('training error', 'testing error'), bty = 'n', cex = 1.25, lwd = 2, lty = c(1, 2), col = c('purple', 'blue'), pch = c(1, 2))


### Exploring model complexity for linear regression and LASSO

require(glmnet)

# Set randomizer seed
set.seed(0)

# Input dimension
p = 15

# How many times to iterate the simulation
n_iterations = 1000

# Size of training data
n_training = 30
n_testing = 30

# True Model: 
#
# b0 = 0, beta_j = 2^(-j)
# X_{i,j} are iid Unif(0, 1)
# Y = XB + E
# where E ~ N(0, sigma^2)

B = matrix(0, nrow = p)
for(j in c(1:p)){
  B[j] = 1/j
}
sigma = 1

lambda_values = seq(from = 0.01, to = 0.91, by = 0.05) 
n_lambda_values = length(lambda_values)

# Initialize results
training_mse = matrix(0, nrow = n_iterations, ncol = n_lambda_values)
testing_mse = matrix(0, nrow = n_iterations, ncol = n_lambda_values)

# Simulate training/testing sets to estimate bias and variance
for(iter in c(1:n_iterations)){
  # Simulate training data
  trainingX = simulate_X(n_training, p)
  trainingY = simulate_Y(trainingX, B, sigma)
  
  testingX = simulate_X(n_testing, p)
  testingY = simulate_Y(testingX, B, sigma)

  x_sd = apply(trainingX, 2, sd)
  x_mean = apply(trainingX, 2, mean)
  
  trainingX = t((t(trainingX) - x_mean) / x_sd)
  testingX = t((t(testingX) - x_mean) / x_sd)
  
  testingY = testingY - mean(trainingY)
  trainingY = trainingY - mean(trainingY)
  
  for(k in c(1:n_lambda_values)){
    lambda = lambda_values[k]
    # LASSO estimation
    fit = glmnet(trainingX, trainingY, alpha = 1, lambda = lambda, intercept = FALSE)
    trainingyHat = predict(fit, trainingX)
    testingyHat = predict(fit, testingX)
    
    # Training/testing MSE
    training_mse[iter, k] = sum((trainingY - trainingyHat)^2) / n_training
    testing_mse[iter, k] = sum((testingY - testingyHat)^2) / n_testing
  }
}  

training_mse = apply(training_mse, 2, mean)
testing_mse = apply(testing_mse, 2, mean)

plot(lambda_values, training_mse, type = 'b', xlab = 'lambda', ylab = 'mse', bty = 'n', main = 'Training/Testing MSE', 
     col = 'purple', pch = 1, lwd = 2, ylim = c(0, 2.2), xlim = c(0, 1), cex.lab = 1.25)
lines(lambda_values, testing_mse, type = 'b', col = 'blue', pch = 2, lwd = 2, lty = 2)

legend(0.5, 2, c('training error', 'testing error'), bty = 'n', cex = 1.25, lwd = 2, lty = c(1, 2), col = c('purple', 'blue'), pch = c(1, 2))



### Exploring model complexity for kNN classification (simple-classification-examples)

euclidean_distance = function(X, Y){
  # Inputs: X and Y are equal length numeric vectors
  # Output: scalar, the euclidean distance between X and Y
  return(sqrt(sum((X - Y)^2)))
}

kNN = function(X, y, xNew, k){
  # Inputs:
  # X is a matrix of training data: one row per datum, one column per input variable
  # y is vector of training outputs
  # xNew is a vector of inputs to be used for prediction 
  # k is an positive integer, number of neighbours
  n = dim(X)[1]
  distance = matrix(0, nrow = n)
  for(i in c(1:n)){
    distance[i] = euclidean_distance(xNew, X[i,])
  }
  sorted_distance = sort(distance)
  # the kth closest neighbour will have a distance of 
  # sorted_distance[k]
  neighbour_index = which(distance <= sorted_distance[k])
  
  # the outputs for my nearest neighbours are y[neighbour_index]
  if(mean(y[neighbour_index]) > 0.5){
    yHat = 1
  }else{
    yHat = 0
  }
  return(yHat)
}


# load the dataset
mydata = read.csv('simple-classification-data.csv')

n = dim(mydata)[1]

Y = mydata$y
X = model.matrix(y ~ 0 + ., data=mydata) 

plot(X[Y == 0, 1], X[Y == 0, 2], cex = 1, lwd=2, pch=1, col='blue', xlab='X1', ylab='X2', bty = 'n', main = 'Scatterplot of (X1, X2)')
points(X[Y == 1,1], X[Y == 1,2], cex = 1, lwd=2, pch=2, col='orange')


x_grid = seq(from=-2, to=6, by=0.1)
y_grid = seq(from=-2, to=6, by=0.1)

n_grid_points = length(x_grid)
grid = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)


k = 70

for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    grid[i, j] = kNN(X, Y, c(x_grid[i], y_grid[j]), k)
  }
}

blue_index = which(grid == 0, arr.ind=TRUE)
orange_index = which(grid == 1, arr.ind=TRUE)


plot(x_grid[blue_index[,1]], y_grid[blue_index[,2]], cex=4, pch='.', col='blue')
points(x_grid[orange_index[,1]], y_grid[orange_index[,2]], cex=4, pch='.', col='orange')



#### LPSA model with CV

require(glmnet)

mydata = read.csv('prostate-data.csv')

dim(mydata)
n = dim(mydata)[1]

# Create folds
nFolds = 5
n_per_fold = ceiling(n / nFolds)

index = c(1:n)
shuffled_index = sample(index, size = n, replace = FALSE)

folds = list()
for(fold in 1:(nFolds - 1)){
  folds[[fold]] = shuffled_index[((fold - 1) * n_per_fold + 1):(fold * n_per_fold)]
}
folds[[nFolds]] = shuffled_index[((nFolds - 1) * n_per_fold + 1):n]

              
# Initialize grid for hyperparameter search                
lambda_values = seq(from = 0, to = 0.02, by = 0.0001)
n_lambda_values = length(lambda_values)

mse = matrix(NA, nrow = nFolds, ncol = n_lambda_values)

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  trainingX = model.matrix(lpsa ~ ., data=training_data)
  testingX = model.matrix(lpsa ~ ., data=testing_data)
  
  trainingY = training_data$lpsa
  testingY = testing_data$lpsa
  
  # Fit  model and record testing error
  for(i in 1:n_lambda_values){
    lambda = lambda_values[i]
    fit = glmnet(trainingX, trainingY, lambda = lambda)
    yHat = predict(fit, newx = testingX)
    mse[fold, i] = mean((yHat - testingY)^2)
  }
}

# Average error over folds
mse = apply(mse, 2, mean)

plot(lambda_values, mse, bty = 'n')

which.min(mse)
lambda_values[48]
min(mse)


### Find all subsets code
expand.grid(rep(list(c(TRUE, FALSE)), 4))





# South African Heart Disease example with AIC

require(glmnet)
mydata = read.csv('SAheart-data.csv', header=T)

n = dim(mydata)[1]
p = dim(mydata)[2] - 1

# Create input and output variables
Y = matrix(mydata$chd, nrow = n)
X = model.matrix(chd ~ 0 + ., data=mydata)

### Scaling inputs
x_sd = apply(X, 2, sd)
x_mean = apply(X, 2, mean)

X = t((t(X) - x_mean) / x_sd)

models = as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), p)))
n_models = dim(models)[1] - 1

aic_values = matrix(0, nrow = n_models)

for(m in c(1:n_models)){
  d = sum(models[m,])
  modelX = cbind(matrix(1, nrow = n), X[, models[m,]])
  
  fit = glmnet(modelX, Y, family="binomial", lambda = 0, alpha = 1)
  aic_values[m] = deviance(fit) + 2*d
}

which.min(aic_values)
models[281,]

names(mydata)[1:p][models[281,]]







### 
# South African Heart Disease - Regularized LDA with Stratified Cross-Validation
###

require(glmnet)
mydata = read.csv('SAheart-data.csv')
mydata$chd = factor(mydata$chd)
head(mydata)

n = dim(mydata)[1]
p = dim(mydata)[2] - 1
classes = levels(mydata$chd)
K = length(classes)

alpha_values = seq(from = 0, to = 1, by = 0.01)
n_alpha_values = length(alpha_values)

mydata0 = mydata[mydata$chd == 0,]
n0 = dim(mydata0)[1]

mydata1 = mydata[mydata$chd == 1,]
n1 = dim(mydata1)[1]

nFolds = 10
n0_per_fold = floor(n0 / nFolds)
n1_per_fold = floor(n1 / nFolds)
folds0 = list()
folds1 = list()

set.seed(0)
shuffled_index0 = sample(c(1:n0))
shuffled_index1 = sample(c(1:n1))
for(fold in c(1:(nFolds - 1))){
  folds0[[fold]] = shuffled_index0[c((1 + (fold - 1) * n0_per_fold):(fold * n0_per_fold))]
  folds1[[fold]] = shuffled_index1[c((1 + (fold - 1) * n1_per_fold):(fold * n1_per_fold))]
}  
folds0[[nFolds]] = shuffled_index0[c((1 + (nFolds - 1) * n0_per_fold):n0)]
folds1[[nFolds]] = shuffled_index1[c((1 + (nFolds - 1) * n1_per_fold):n1)]

accuracy = matrix(0, nrow = n_alpha_values, ncol = nFolds)

for(fold in c(1:nFolds)){
  
  training_data = rbind(mydata0[-folds0[[fold]],], mydata1[-folds1[[fold]],])
  testing_data = rbind(mydata0[folds0[[fold]],], mydata1[folds1[[fold]],])
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  # Create input and output variables
  trainingY = training_data$chd
  trainingX = model.matrix(chd ~ ., data=training_data)
  trainingX0 = trainingX[, -1]
  
  # For predictions, create test input and output variables
  testingY = testing_data$chd
  testingX = model.matrix(chd ~ ., data=testing_data)
  testingX0 = testingX[, -1]
  
  # Estimate muHat
  muHat = matrix(0, nrow = K, ncol = p)
  for(k in c(1:K)){
    muHat[k,] = apply(trainingX0[trainingY == classes[k],], 2, mean)
  }
  
  # Pooled estimate covariance matrix
  SigmaHat = pooled_cov(trainingX0, trainingY, muHat)
  
  # Scale the training and testing data
  x_sd = sqrt(diag(SigmaHat))
  trainingX0 = t(t(trainingX0) / x_sd)
  testingX0 = t(t(testingX0) / x_sd)
  
  # Re-estimate muHat 
  muHat = matrix(0, nrow = K, ncol = p)
  for(k in c(1:K)){
    muHat[k,] = apply(trainingX0[trainingY == classes[k],], 2, mean)
  }
  
  # Re-estimate pooled covariance matrix
  SigmaHat = pooled_cov(trainingX0, trainingY, muHat)
  
  # Estimate class probabilities
  q = matrix(0, nrow = K)
  for(k in c(1:K)){
    q[k] = mean(trainingY == classes[k])
  }
  
  for(a in c(1:n_alpha_values)){
    alpha = alpha_values[a]
    
    # Regularized estimate of Sigma
    SigmaHat_A = (1 - alpha) * SigmaHat + alpha*diag(p)
    
    # Pre-compute inverse for computational savings
    invSigmaHat = solve(SigmaHat_A)
    
    # Compute discriminant values for testing data
    delta = matrix(0, nrow = n_testing, ncol = K)
    for(i in c(1:n_testing)){ 
      for(k in c(1:K)){
        delta[i,k] = q[k] * exp( -(1/2)*(t(testingX0[i,]) - muHat[k,]) 
                                 %*% invSigmaHat 
                                 %*% t(t(testingX0[i,]) - muHat[k,])) 
      }
    }
    
    yHat = apply(delta, 1, which.max)
    yHat = classes[yHat]
    accuracy[a, fold] = mean(yHat == testingY)
  }
}

accuracy = apply(accuracy, 1, mean)
plot(alpha_values, accuracy)

which.max(accuracy)
alpha_values[which.max(accuracy)]
max(accuracy)


