data = read.csv('piecewise-data.csv')
x = data$X
y = data$Y
# spline methods
library(splines)

# cubic spline-------------------------------------------------
ordr = order(x)
x = x[ordr]
y = y[ordr]

knot = c(0.25, 0.5, 0.75) # example
H = bs(x, knots = knot, degree = 3, intercept = TRUE)
model = lm(y ~ 0 + ., data=H)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)

H0 = bs(x0, knots = knot, degree = 3, intercept = TRUE) 

y0 = predict(model, H0)
plot(x,y)
lines(x0, y0, col='green', lwd=2, lty = 2)

# Make prediction to new data
H_new = bs(0.6, knots = knot, degree = 3, intercept = TRUE)
y_pred = predict(model, newdata = H_new)
y_pred

# natural spline--------------------------------------------
ordr = order(x)
x = x[ordr]
y = y[ordr]

knot = c(0.25, 0.5, 0.75) # example
H = ns(x,df=3, knots = knot, intercept=TRUE)
model = lm(y ~ 0 + ., data=H)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)
H0 = ns(x0,df=3, knots = knot, intercept=TRUE)

y0 = predict(model, H0)
plot(x,y)
lines(x0, y0, col='green', lwd=2, lty = 2)

###
# Concrete Strength - Natural Spline Example  
###

mydata = read.csv('concrete-data.csv')
n = dim(mydata)[1]
mydata$age = mydata$age + rnorm(n, mean = 0, sd = 0.4)

# Set up folds
nFolds = 10
n_per_fold = floor(n / nFolds)

set.seed(0)
folds = list()
shuffled_index = sample(c(1:n))
for(k in c(1:nFolds)){
  folds[[k]] = shuffled_index[c((1 + (k - 1) * n_per_fold):(k * n_per_fold))]
}  

# Set up df's
df_levels = c(2:20)
n_df = length(df_levels)

# Initialize mse
testing_mse = matrix(0, nrow = n_df, ncol = nFolds)

for(fold in c(1:nFolds)){
  training_data = mydata[-folds[[fold]],]
  n_training = dim(training_data)[1]
  
  testing_data = mydata[folds[[fold]],]
  n_testing = dim(testing_data)[1]
  
  # Define X and Y 
  trainingX = training_data$age
  trainingY = training_data$strength
  
  testingX = testing_data$age
  testingY = testing_data$strength
  
  testingX = (testingX - mean(trainingX)) / sd(trainingX)
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX)
  
  for(i in c(1:n_df)){
    df = df_levels[i]
    trainingN = ns(trainingX, df = df, intercept = TRUE)
    model = lm(trainingY ~ 0 + ., data=trainingN)
    
    testingN = ns(testingX, knots = attr(trainingN, 'knots'), 
                  Boundary.knots = attr(trainingN, 'Boundary.knots'), 
                  intercept = TRUE) 
    testingYhat = predict(model, newdata = testingN)
    testing_mse[i, fold] = mean((testingY - testingYhat)^2)
  }
}

testing_mse = apply(testing_mse, 1, mean)
testing_mse

df = df_levels[which.min(testing_mse)]
df

# Define X and Y 
X = mydata$age
Y = mydata$strength

X = (X - mean(X)) / sd(X)

N = ns(X, df = df, intercept = TRUE)
model = lm(Y ~ 0 + ., data=N)

x0 = seq(from = min(X), to = max(X), by = 0.01)
N0 = ns(x0, knots = attr(N, 'knots'), 
        Boundary.knots = attr(N, 'Boundary.knots'), 
        intercept = TRUE) 
y0 = predict(model, N0)

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Age', bty = 'n')
lines(x0, y0, lwd = 2, col = 'red')

# smoothing spline-------------------------------------
ordr = order(x)
x = x[ordr]
y = y[ordr]

# Fit smoothing spline
spline_model = smooth.spline(x, y)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)
y0 = predict(spline_model, x0)$y

# Create plot
plot(x, y, main="Smoothing Spline Fit")
lines(x0, y0, col='green', lwd=2, lty=2)

# Cross validated smoothing spline
mydata = read.csv('concrete-data.csv')
n = dim(mydata)[1]

# Set up folds
nFolds = 10
n_per_fold = floor(n / nFolds)

set.seed(0)
folds = list()
shuffled_index = sample(c(1:n))
for(k in c(1:nFolds)){
  folds[[k]] = shuffled_index[c((1 + (k - 1) * n_per_fold):(k * n_per_fold))]
}  

# Set up df's
lambda_levels = seq(from = 0.001, to = 0.1, by = 0.001)
n_lambda = length(lambda_levels)

# Initialize mse
testing_mse = matrix(0, nrow = n_lambda, ncol = nFolds)

for(fold in c(1:nFolds)){
  training_data = mydata[-folds[[fold]],]
  n_training = dim(training_data)[1]
  
  testing_data = mydata[folds[[fold]],]
  n_testing = dim(testing_data)[1]
  
  # Define X and Y 
  trainingX = training_data$age
  trainingY = training_data$strength
  
  testingX = testing_data$age
  testingY = testing_data$strength
  
  testingX = (testingX - mean(trainingX)) / sd(trainingX)
  trainingX = (trainingX - mean(trainingX)) / sd(trainingX)
  
  for(i in c(1:n_lambda)){
    lambda = lambda_levels[i]
    fit = smooth.spline(X, Y, lambda = lambda)
    testingYhat = predict(fit, testingX)$y
    testing_mse[i, fold] = mean((testingY - testingYhat)^2)
  }
}

testing_mse = apply(testing_mse, 1, mean)
testing_mse

lambda = lambda_levels[which.min(testing_mse)]
lambda

# Define X and Y 
X = mydata$age
Y = mydata$strength

X = (X - mean(X)) / sd(X)
fit = smooth.spline(X, Y, lambda = lambda)
fit$df

x0 = seq(from = min(X), to = max(X), by = 0.01)
y0 = predict(fit, x0)$y

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Age', bty = 'n')
lines(x0, y0, col = 'red', lwd = 2)


# _______________________________________________

# kernel methods

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

# kernel regression / classification

# Regression example
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
  y0[i] = kernel_smoothing(x0[i], X, Y, uniform_kernel, constant_bandwidth, 2)
}
lines(x0, y0, col = 'blue', lwd = 3)

#classification example
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
    delta[i, k] = kernel_smoothing(X[i], X, indicatorY[,k], epanechnikov_kernel, constant_bandwidth, 2)
  }
}
yHat = levels(iris$Species)[apply(delta, 1, which.max)]
mean(yHat == Y)


n0 = 200
x0 = seq(from = min(iris$Petal.Length), to = max(iris$Petal.Length), length.out = n0)
y0 = matrix(NA, nrow = n0, ncol = K)

for(k in c(1:K)){
  for(i in c(1:n0)){
    y0[i, k] = kernel_smoothing(x0[i], X, indicatorY[,k], epanechnikov_kernel, constant_bandwidth, 2)
  }
}


plot(x0, y0[,1], bty = 'n', type = 'l', col = 'red', 
     lwd = 2, xlab = 'Petal Length', ylab = 'delta')

lines(x0, y0[,2], col = 'dodgerblue3', lwd = 2)
lines(x0, y0[,3], col = 'forestgreen', lwd = 2)

points(iris$Petal.Length, indicatorY[,1], col = 'red', cex = 1, lwd = 2)
points(iris$Petal.Length, indicatorY[,2], col = 'dodgerblue3', cex = 1, lwd = 2)



# local linear regression
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
# Cross-validation------------------------------------------------------------------------

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

# K-fold
# Stratified
# (Assignment function)

