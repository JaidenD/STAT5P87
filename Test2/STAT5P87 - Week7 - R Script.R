### STAT5P87 - Week7.R ###

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


#########
### Prostate Data - kNN regression with CV
#########

kNN_regression = function(X, Y, x_new, k){ 
  # Inputs to my kNN function
  #   X - matrix of training inputs (n x p)
  #   Y - matrix of training outputs 
  #   x_new - observation to classify
  #   k - number of neighbours to consider
  # Output
  #   predicted value of y_new
  n = length(Y)	
  distances = matrix(0, nrow = n)
  
  for(i in c(1:n)){
    distances[i] = abs(X[i,] - x_new)
  }
  
  sorted_distances = sort(distances)
  sorted_distances[k]
  
  my_kNN = which(distances <= sorted_distances[k])
  
  return(mean(Y[my_kNN]))
}



### Load data frame
mydata = read.csv('prostate-data.csv', header=T)

### For a sequence of lambda values 
k_values = seq(from = 1, to = 30, by = 1)
n_k_values = length(k_values)

### Split into training/testing sets
n = dim(mydata)[1]
nFolds = 5
folds = make_folds(n, nFolds, stratified = FALSE, seed = 0)
  


# Initialize MSE values for the ridge regression
mse = matrix(0, nrow=n_k_values, ncol = nFolds)


for(fold in c(1:nFolds)){
  
  # define training/testing data for the given fold
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  ### Create input matrix and output vector for training and testing data
  trainingX = matrix(training_data$lcavol, nrow = n_training) 
  trainingY = matrix(training_data$lpsa, nrow=n_training)
  
  testingX = matrix(testing_data$lcavol, nrow = n_testing)
  testingY = matrix(testing_data$lpsa, nrow = n_testing)
  
  # Scale inputs
  testingX = (testingX - mean(trainingX)) / sd(trainingX) 
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX) 
  
  # Perform kNN regression for a range of k values
  for(i in c(1:n_k_values)){ 
    testingYhat = matrix(0, nrow = n_testing)
    k = k_values[i]
    for(j in c(1:n_testing)){
      testingYhat[j] = kNN_regression(trainingX, trainingY, testingX[j], k) 
    }
    mse[i, fold] = sum((testingYhat - testingY)^2) / n_testing
  }
}

mse = apply(mse, 1, mean)

# Plot MSE values as a function of lambda
plot(k_values, mse, xlab = 'k', ylab = 'MSE', main = 'kNN Regression - MSE of testing data', bty = 'n')

# Which lambda value minimizes MSE?
k_values[which.min(mse)]
k = k_values[which.min(mse)]

X = mydata$lcavol
X = scale(X)
Y = mydata$lpsa

n0 = 200
x0 = seq(from = min(X), to = max(X), by = diff(range(X)) / (n0 - 1))
y0 = matrix(0, nrow = n0)
for(j in c(1:n0)){
  y0[j] = kNN_regression(X, Y, x0[j], k) 
}

plot(X, Y, xlab = 'lcavol', ylab = 'lpsa', main = 'Scatterplot of lcavol vs. lpsa', bty = 'n', lwd = 2)
lines(x0, y0, col = 'red', lwd = 2)



###########
# Define Kernels
###########

# Uniform Kernel

t = seq(from=-2, to=2, by = 0.01)
uniform_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Uniform kernel
  return(as.integer(abs(t) <= 1) / 2)
}

y = uniform_kernel(t)

plot(t, y, type = 'l', lwd = 2,
     xlab = 't', ylab = 'D(t)', main = 'Uniform Kernel', 
     bty = 'n', ylim = c(0, 1), xlim = c(-2, 2))



# Epanechnikov Kernel

t = seq(from=-2, to=2, by = 0.01)

epanechnikov_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Epanechnikov kernel
  return(as.integer(abs(t) <= 1) * (3/4) * (1 - t^2))
}

y = epanechnikov_kernel(t)

plot(t, y, type = 'l', lwd = 2,
     xlab = 't', ylab = 'D(t)', main = 'Epanechnikov Kernel', 
     bty = 'n', ylim = c(0, 1), xlim = c(-2, 2))


# Tri-cube Kernel

t = seq(from=-2, to=2, by = 0.01)

tricube_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Tricube kernel
  return(as.integer(abs(t) <= 1) * (1 - abs(t)^3)^3)
}

y = tricube_kernel(t)

plot(t, y, type = 'l', lwd = 2,
     xlab = 't', ylab = 'D(t)', main = 'Tri-Cube Kernel', 
     bty = 'n', ylim = c(0, 1), xlim = c(-2, 2))


# Gaussian Kernel

t = seq(from=-2, to=2, by = 0.01)

gaussian_kernel = function(t){
  # Input: t - real number
  # Output: D(t) - real number, where D() is the Gaussian kernel
  return((2*pi)^(-1/2) * exp(-t^2/2))
}

y = gaussian_kernel(t)

plot(t, y, type = 'l', lwd = 2,
     xlab = 't', ylab = 'D(t)', main = 'Gaussian Kernel', 
     bty = 'n', ylim = c(0, 1), xlim = c(-2, 2))



### 
# Write a Kernel smoothing function
###

kernel_smoothing = function(x0, X, Y, K, h = 0.25){
  # Inputs
  #   x0 - input to be predicted
  #   X - matrix of training inputs (n x p)
  #   Y - matrix of training outputs (n x 1)
  #   k - kernel function
  #   h - constant bandwidth 
  #
  # Outputs
  #   predicted y0 value   
  
  # Use the kernel to get weights
  w = K(abs(x0 - X)/h)
  return(sum(w*Y) / sum(w))
}



###
# Applying Kernel Methods to piecewise example data with constant bandwidth h(x) = 0.25
###

mydata = read.csv('piecewise-data.csv')
X = mydata$X
Y = mydata$Y
n = length(Y)

# For plotting f(X)
x0 = seq(from=0, to=1, by=0.001)
n0 = length(x0)
y0 = matrix(0, nrow=n0)

# Plot data along with f(X) for comparison
plot(X, Y, ylim = c(-3, 3), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')

# Uniform Kernel 
for(i in c(1:n0)){
  y0[i] = kernel_smoothing(x0[i], X, Y, uniform_kernel)
}
lines(x0, y0, col = 'blue', lwd = 3)


# Epanechnkiov Kernel 
for(i in c(1:n0)){
  y0[i] = kernel_smoothing(x0[i], X, Y, epanechnikov_kernel)
}
lines(x0, y0, col = 'green', lwd = 3)

# Tri-Cube Kernel 
for(i in c(1:n0)){
  y0[i] = kernel_smoothing(x0[i], X, Y, tricube_kernel)
}
lines(x0, y0, col = 'red', lwd = 3)

# Gaussian Kernel 
for(i in c(1:n0)){
  y0[i] = kernel_smoothing(x0[i], X, Y, gaussian_kernel)
}
lines(x0, y0, col = 'purple', lwd = 3)




###
# Iris Example
###

data(iris)
head(iris)

K = 3
n = dim(iris)[1]

X = iris$Petal.Length
Y = matrix(iris$Species, nrow = n)

indicatorY = matrix(0, nrow = n, ncol = K)
for(k in c(1:K)){
  indicatorY[,k] = as.integer(iris$Species == levels(iris$Species)[k])
}


## Epanechnikov kernel
delta = matrix(0, nrow = n, ncol = K)
for(k in c(1:K)){
  for(i in c(1:n)){
    delta[i, k] = kernel_smoothing(X[i], X, indicatorY[,k], epanechnikov_kernel, h=2)
  }
}
yHat = levels(iris$Species)[apply(delta, 1, which.max)]
mean(yHat == Y)


n0 = 200
x0 = seq(from = min(iris$Petal.Length), to = max(iris$Petal.Length), length.out = n0)
y0 = matrix(NA, nrow = n0, ncol = K)

for(k in c(1:K)){
  for(i in c(1:n0)){
    y0[i, k] = kernel_smoothing(x0[i], X, indicatorY[,k], epanechnikov_kernel, h=2)
  }
}


plot(x0, y0[,1], bty = 'n', type = 'l', col = 'red', 
     lwd = 2, xlab = 'Petal Length', ylab = 'delta')

lines(x0, y0[,2], col = 'dodgerblue3', lwd = 2)
lines(x0, y0[,3], col = 'forestgreen', lwd = 2)

points(iris$Petal.Length, indicatorY[,1], col = 'red', cex = 1, lwd = 2)
points(iris$Petal.Length, indicatorY[,2], col = 'dodgerblue3', cex = 1, lwd = 2)



####################
# Prostate Cancer Example 
####################

constant_bandwidth = function(lambda, x0=NULL, X=NULL){
  # Input: lambda - positive real number
  # additional inputs are for consistency with adaptive bandwidth function
  return(lambda)
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
  w = K(abs(x0 - X)/h(lambda, x0, X))
  return(sum(w*Y) / sum(w))
}


# Load data and define folds

mydata = read.csv('prostate-data.csv')

n = dim(mydata)[1]
nFolds = 5
folds = make_folds(n, nFolds)


## Constant bandwidth models 

# Define the set of lambda values
constant_lambda_values = seq(from = 0.015, to = 0.8, by = 0.005)
n_constant_lambda_values = length(constant_lambda_values)
constant_mse = matrix(0, nrow = n_constant_lambda_values, ncol = nFolds)

# Estimate MSE for each fold/lambda
for(fold in 1:nFolds){
  
  # Define training / testing dataframes based on fold
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]

  # Define training and testing data matrices
  trainingX = training_data$lcavol
  trainingY = training_data$lpsa
  
  testingX = testing_data$lcavol
  testingY = testing_data$lpsa
  
  # Scale inputs
  testingX = (testingX - mean(trainingX)) / sd(trainingX)
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX)
  
  # Predict testing Y using the kernel smoother with constant bandwidth 
  for(i in c(1:n_constant_lambda_values)){
    lambda = constant_lambda_values[i]
    testingYhat = matrix(0, nrow = n_testing)
    for(j in c(1:n_testing)){
      testingYhat[j] = kernel_smoothing(testingX[j], trainingX, trainingY, gaussian_kernel, 
                                        constant_bandwidth, lambda = lambda)
    }
    # Compute MSE
    constant_mse[i, fold] = sum((testingY - testingYhat)^2) / n_testing
  }
}

# Average across folds
constant_mse = apply(constant_mse, 1, mean)

plot(constant_lambda_values, constant_mse)

constant_lambda = constant_lambda_values[which.min(constant_mse)]
constant_min_mse = min(constant_mse)



## Adaptive bandwidth models 

# Define set of lambda values
adaptive_lambda_values = seq(from = 2, to = 25, by = 1)
n_adaptive_lambda_values = length(adaptive_lambda_values)
adaptive_mse = matrix(0, nrow = n_adaptive_lambda_values, ncol = nFolds)

# Estimate MSE for each fold/lambda
for(fold in 1:nFolds){
  
  # Define training / testing dataframes based on fold
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  # Define training and testing data matrices
  trainingX = training_data$lcavol
  trainingY = training_data$lpsa
  
  testingX = testing_data$lcavol
  testingY = testing_data$lpsa
  
  # Scale inputs
  testingX = (testingX - mean(trainingX)) / sd(trainingX)
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX)
  
  # Predict testing Y using the kernel smoother with adaptive bandwidth 
  for(i in c(1:n_adaptive_lambda_values)){
    lambda = adaptive_lambda_values[i]
    testingYhat = matrix(0, nrow = n_testing)
    for(j in c(1:n_testing)){
      testingYhat[j] = kernel_smoothing(testingX[j], trainingX, trainingY, gaussian_kernel, 
                                        adaptive_bandwidth, lambda = lambda)
    }
    # Compute MSE
    adaptive_mse[i, fold] = sum((testingY - testingYhat)^2) / n_testing
  }
}

# Average across folds
adaptive_mse = apply(adaptive_mse, 1, mean)

plot(adaptive_lambda_values, adaptive_mse)

adaptive_lambda = adaptive_lambda_values[which.min(adaptive_mse)]
adaptive_min_mse = min(adaptive_mse)



## Adaptive LLR bandwidth models 

# Define set of lambda values
adaptiveLLR_lambda_values = seq(from = 4, to = 45, by = 1)
n_adaptiveLLR_lambda_values = length(adaptiveLLR_lambda_values)
adaptiveLLR_mse = matrix(0, nrow = n_adaptiveLLR_lambda_values, ncol = nFolds)

# Estimate MSE for each fold/lambda
for(fold in 1:nFolds){
  
  # Define training / testing dataframes based on fold
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  # Define training and testing data matrices
  trainingX = training_data$lcavol
  trainingY = training_data$lpsa
  
  testingX = testing_data$lcavol
  testingY = testing_data$lpsa
  
  # Scale inputs
  testingX = (testingX - mean(trainingX)) / sd(trainingX)
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX)
  
  # Predict testing Y using the kernel smoother with adaptive bandwidth 
  for(i in c(1:n_adaptiveLLR_lambda_values)){
    lambda = adaptiveLLR_lambda_values[i]
    testingYhat = matrix(0, nrow = n_testing)
    for(j in c(1:n_testing)){
      testingYhat[j] = local_linear_regression(testingX[j], trainingX, trainingY, gaussian_kernel, 
                                        adaptive_bandwidth, lambda = lambda)
    }
    # Compute MSE
    adaptiveLLR_mse[i, fold] = sum((testingY - testingYhat)^2) / n_testing
  }
}

# Average across folds
adaptiveLLR_mse = apply(adaptiveLLR_mse, 1, mean)

plot(adaptiveLLR_lambda_values, adaptiveLLR_mse)

adaptiveLLR_lambda = adaptiveLLR_lambda_values[which.min(adaptiveLLR_mse)]
adaptiveLLR_min_mse = min(adaptiveLLR_mse)





# Define inputs and outputs for whole data set (to plot)
X = mydata$lcavol
Y = mydata$lpsa

x0 = seq(from = min(X), to = max(X), by = 0.01)
n0 = length(x0)
y0_constant = matrix(0, nrow = n0)
y0_adaptive = matrix(0, nrow = n0)
y0_adaptiveLLR = matrix(0, nrow = n0)

for(j in c(1:n0)){
  y0_constant[j] = kernel_smoothing(x0[j], X, Y, gaussian_kernel, 
                                    constant_bandwidth, lambda = constant_lambda)
}

for(j in c(1:n0)){
  y0_adaptive[j] = kernel_smoothing(x0[j], X, Y, gaussian_kernel, 
                                    adaptive_bandwidth, lambda = adaptive_lambda)
}

for(j in c(1:n0)){
  y0_adaptiveLLR[j] = local_linear_regression(x0[j], X, Y, gaussian_kernel, 
                                    adaptive_bandwidth, lambda = adaptiveLLR_lambda)
}

plot(X, Y)
lines(x0, y0_constant, lwd = 2, col = 'red')
lines(x0, y0_adaptive, lwd = 2, col = 'blue')
lines(x0, y0_adaptiveLLR, lwd = 2, col = 'green')

constant_min_mse
adaptive_min_mse






#########
### Boundary Example 
#########

mydata = read.csv('boundary-example.csv')

X = mydata$x
Y = mydata$y
n = length(Y)

# For plotting f(X)
x0 = seq(from=0, to=1, by=0.001)
n0 = length(x0)
y0 = matrix(0, nrow=n0)

# Plot data along with f(X) for comparison
plot(X, Y, ylim = c(-0.5, 0.1), bty='n', xlab = 'X', ylab = 'Y', main = 'dataset43')

# Epanechnkiov Kernel 
for(i in c(1:n0)){
  y0[i] = kernel_smoothing(x0[i], X, Y, epanechnikov_kernel, constant_bandwidth, 0.25)
}
lines(x0, y0, col = 'green', lwd = 2)




local_linear_regression = function(x0, X, Y, K, h, lambda = 2){
  # Inputs
  #   x0 - input to be predicted
  #   X - matrix of training inputs (n x p)
  #   Y - matrix of training outputs (n x 1)
  #   k - kernel function
  #   h - bandwidth function
  #
  # Outputs
  #   predicted y0 value   
  
  n = length(Y)
  bandwidth = h(lambda, x0, X)
  w = K(abs(x0 - X)/bandwidth)
  if(sum(w > 0) < 3){
    stop('local regression requires at least 2 non-zero weights')
  }
  w = as.vector(w / sum(w))
  W = diag(w)
  X = cbind(matrix(1, nrow = n), X)
  hatB = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
  return(c(1, x0) %*% hatB)
}

for(i in c(1:n0)){
  y0[i] = local_linear_regression(x0[i], X, Y, epanechnikov_kernel, constant_bandwidth, 0.25)
}
lines(x0, y0, col = 'red', lwd = 2)



#########
### South African Heart Disease Example - Local Logistic Regression ###
#########

require(glmnet)

mydata = read.csv('SAheart-data.csv', header=T)
head(mydata)

n = dim(mydata)[1]

nFolds = 5
folds = make_folds(mydata$chd, nFolds, stratified = TRUE)

lambda_values = seq(from = 1, to = 5, by = 0.2)
n_lambda_values = length(lambda_values)
accuracy = matrix(0, nrow = nFolds, ncol = n_lambda_values)

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  # Define training and testing data matrices
  trainingX = model.matrix(chd ~ sbp, data=training_data) 
  trainingY = matrix(training_data$chd, nrow = n_training)
  
  testingX = model.matrix(chd ~ sbp, data=testing_data)
  testingY = testing_data$chd
  
  testingX[,2] = (testingX[,2] - mean(trainingX[,2])) / sd(trainingX[,2])
  trainingX[,2] = (trainingX[,2] - mean(trainingX[,2])) / sd(trainingX[,2])
  
  for(i in 1:n_lambda_values){
    
    lambda = lambda_values[i]
    testingYhat = matrix(0, nrow = n_testing)
    for(j in c(1:n_testing)){
      w = tricube_kernel(abs(testingX[j, 2] - trainingX[, 2])/lambda)
      w = as.vector(w / sum(w))
      fit = glmnet(trainingX, trainingY, family="binomial", lambda = 0, weights = w)
      testingYhat[j] = predict(fit, newx=matrix(testingX[j,], nrow = 1), type = 'class')
    }
    accuracy[fold, i] = mean(testingY == testingYhat)
  }
}

accuracy = apply(accuracy, 2, mean)
plot(lambda_values, accuracy)

lambda = lambda_values[which.max(accuracy)]

# Create input and output variables
Y = mydata$chd
X = model.matrix(chd ~ sbp, data=mydata)

X[,2] = (X[,2] - mean(X[,2])) / sd(X[,2])
yHat = matrix(0, nrow = n)

for(i in c(1:n)){
  w = tricube_kernel(abs(X[i,2] - X[,2])/lambda)
  w = as.vector(w / sum(w))
  fit = glmnet(X, Y, family="binomial", lambda = 0, weights = w)
  yHat[i] = predict(fit, matrix(X[i,], nrow = 1), type = 'response')
}

plot(X[,2], yHat)








####################
# Concrete Example - Multivariate Kernel Smoothing
####################

multivariate_kernel_smoothing = function(x0, X, Y, K, lambda = 1){
  # Inputs
  #   x0 - input to be predicted
  #   X - training inputs (n x p) matrix
  #   Y - training outputs (n x 1) matrix
  #   K - kernel (function)
  #   lambda - smoothing parameter (numeric)
  #
  # Outputs
  #   predicted value of y0
  
  distance = function(a, b){
    return(sqrt(sum((a - b)^2)))
  }
  
  # Determine the number of samples
  N = dim(X)[1]
  w = matrix(0, nrow = N)
  for(i in c(1:N)){
    w[i] = K(distance(x0, X[i,])/lambda)
  }
  return(sum(w*Y) / sum(w))
}


mydata = read.csv('concrete-data.csv', header=T)
head(mydata)

n = dim(mydata)[1]
nFolds = 10
p = dim(mydata)[2] - 1

folds = make_folds(n, nFolds)

### For a sequence of lambda values 
lambda_values = seq(from = 0.1, to = 0.8, by = 0.05)
n_lambda_values = length(lambda_values)

# Initialize MSE values for the ridge regression
MSE = matrix(0, nrow=n_lambda_values, ncol = nFolds)

for(fold in c(1:nFolds)){
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  ### Create input matrix and output vector for training and testing data
  trainingX = model.matrix(strength ~ 0 + ., data=training_data)
  trainingY = matrix(training_data$strength, nrow=n_training)
  
  testingX = model.matrix(strength ~ 0 + ., data=testing_data)
  testingY = matrix(testing_data$strength, nrow=n_testing)
  
  ### Scaling training and testing inputs
  x_sd = apply(trainingX, 2, sd)
  x_mean = apply(trainingX, 2, mean)
  
  trainingX = t((t(trainingX) - x_mean) / x_sd)
  testingX = t((t(testingX) - x_mean) / x_sd)
  
  # Perform ridge regression for a range of lambda values
  for(i in c(1:n_lambda_values)){ 
    lambda = lambda_values[i]
    testingYhat = matrix(0, nrow = n_testing)
    for(j in c(1:n_testing)){
      testingYhat[j] = multivariate_kernel_smoothing(testingX[j,], trainingX, trainingY, gaussian_kernel, lambda = lambda)
    }
    MSE[i, fold] = sum((testingYhat - testingY)^2) / n_testing
  }
}

# Take average MSE across folds
MSE = apply(MSE, 1, mean)
plot(lambda_values, MSE, xlab = expression(lambda), main = 'MSE from cross validation', bty = 'n')

lambda_values[which.min(MSE)]
min(MSE)









