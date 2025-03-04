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
      # Alternative for linear regression
      # fit = glmnet(trainingX, trainingY, family="gaussian", lambda = 0, weights = w)
      testingYhat[j] = predict(fit, newx=matrix(testingX[j,], nrow = 1), type = 'class')
      # Alternative for linear regression
      # testingYhat[j] = predict(fit, newx=matrix(testingX[j,], nrow = 1))
    }
    accuracy[fold, i] = mean(testingY == testingYhat)
  }
}

accuracy = apply(accuracy, 2, mean)
# Alternative for linear regression
# mse[fold, i] = mean((testingY - testingYhat)^2)
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




















########## Week 6 ALL ########################
### STAT5P87 - Week6.R ###

## Plot Masking data

mydata = read.csv('masking-example.csv')
head(mydata)

n = dim(mydata)[1]

Y = as.matrix(mydata$Y, nrow = n)
X = model.matrix(Y ~ 0 + X1 * X2 + ., data=mydata)
p = dim(X)[2]

x1min = min(X[,1])
x1max = max(X[,1])

x2min = min(X[,2])
x2max = max(X[,2])

plot(X[Y == 1, 1], X[Y == 1, 2], col = 'forestgreen', pch = 2, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(X[Y == 2, 1], X[Y == 2, 2], col = 'darkorange1', pch = 2, lwd=2)
points(X[Y == 3, 1], X[Y == 3, 2], col = 'red', pch = 2, lwd=2)

legend(-0.2, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)

Y1 = as.integer(Y == 1)
Y2 = as.integer(Y == 2)
Y3 = as.integer(Y == 3)
allY = cbind(Y1, Y2, Y3)

###
# Perform linear regression 
###

X = model.matrix(Y ~ X1 * X2 + ., data=mydata)

bHat = solve(t(X) %*% X) %*% t(X) %*% allY

allYhat = X %*% bHat

yHat = apply(allYhat, 1, which.max)

mean(yHat == Y)

xtabs(~ Y + yHat)


####################################################
# Piecewise polynomial example
####################################################

# Load data
mydata = read.csv('piecewise-data.csv')

X = mydata$X
Y = mydata$Y
n = length(Y)

plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
abline(lm(Y ~ X), lwd=2)



# knots
xi = c(-Inf, 1/3, 2/3, Inf)
K = 2

###
# Piecewise constant
###

h1 = as.integer(X < xi[2])
h2 = as.integer(X < xi[3] & X >= xi[2])
h3 = as.integer(X >= xi[3])
H = data.frame(cbind(Y, h1, h2, h3))

model = lm(Y ~ 0 + ., data=H)

plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise constant example')
lines(c(0, xi[2]), c(model$coefficient[1], model$coefficient[1]), col='forestgreen', lwd=2)
lines(c(xi[2], xi[3]), c(model$coefficient[2], model$coefficient[2]), col='forestgreen', lwd=2)
lines(c(xi[3], 1), c(model$coefficient[3], model$coefficient[3]), col='forestgreen', lwd=2)



###
# Piecewise linear
###

h1 = as.integer(X < xi[2])
h2 = as.integer(X < xi[3] & X >= xi[2])
h3 = as.integer(X >= xi[3])
h4 = X * as.integer(X < xi[2])
h5 = X * as.integer(X < xi[3] & X >= xi[2])
h6 = X * as.integer(X >= xi[3])

H = data.frame(cbind(Y, h1, h2, h3, h4, h5, h6))

model = lm(Y ~ 0 + ., data=H)

plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(c(0, xi[2]), c(model$coefficient[1], model$coefficient[1] + xi[2] * model$coefficient[4]), col='blue', lwd=2)
lines(c(xi[2], xi[3]), c(model$coefficient[2] + xi[2] * model$coefficient[5], model$coefficient[2] + xi[3] * model$coefficient[5]), col='blue', lwd=2)
lines(c(xi[3], 1), c(model$coefficient[3] + xi[3] * model$coefficient[6], model$coefficient[3] + model$coefficient[6]), col='blue', lwd=2)





###
# Piecewise linear (continuous)
###

# A global linear basis function
h1 = 1
h2 = X

# Changes to the slope after each knot
h3 = ifelse(X - xi[2] >= 0, X - xi[2], 0)
h4 = ifelse(X - xi[3] >= 0, X - xi[3], 0)

H = data.frame(cbind(Y, h1, h2, h3, h4))

# Use linear methods to fit basis expanded model
model = lm(Y ~ 0 + ., data=H)

# Create a basis expansion for plotting
x0 = seq(from=0, to=1, by=0.01)
h10 = x0*0 + 1
h20 = x0
h30 = ifelse(x0 - xi[2] >= 0, x0 - xi[2], 0)
h40 = ifelse(x0 - xi[3] >= 0, x0 - xi[3], 0)

# Plot fit model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
y0 = h10*model$coefficient[1] + model$coefficient[2]*h20 + model$coefficient[3]*h30 + model$coefficient[4]*h40
lines(x0, h10*model$coefficient[1] + model$coefficient[2]*h20 + model$coefficient[3]*h30 + model$coefficient[4]*h40, lwd = 2, col='purple') 




###
# Piecewise Cubic
###

M = 3
H = matrix(0, nrow = n, ncol = (K+1)*(M+1))

for(k in c(1:(K+1))){
  for(m in c(0:M)){
    H[, k + m*(K+1)] = as.integer(X > xi[k] & X <= xi[k + 1]) * X^m
  }
} 

H = data.frame(H)

model = lm(Y ~ 0 + ., data=H)

x0 = seq(from=0, to=1, by=0.01)
n0 = length(x0)
H0 = matrix(0, nrow = n0, ncol = (K+1)*(M+1))

for(k in c(1:(K+1))){
  for(m in c(0:M)){
    H0[, k + m*(K+1)] = as.integer(x0 > xi[k] & x0 <= xi[k + 1]) * x0^m
  }
} 

H0 = data.frame(H0)
y0 = predict(model, H0)

plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0[x0 < xi[2]], y0[x0 < xi[2]], col='red', lwd=2)
lines(x0[x0 < xi[3] & x0 >= xi[2]], y0[x0 < xi[3] & x0 >= xi[2]], col='red', lwd=2)
lines(x0[x0 >= xi[3]], y0[x0 >= xi[3]], col='red', lwd=2)





###
# Piecewise Cubic (continuous)
###

M = 3
K = 2

# Create basis expansion
H = matrix(0, nrow = n, ncol = M*(K + 1) + 1)

# Global terms 
H[,1] = 1
for(m in c(1:M)){
  H[,m + 1] = X^m
}

# Add a j^th order term after the i^th knot
# No 0^th order terms to ensure continuity
for(k in c(1:K)){
  for(m in c(1:M)){
    H[, k*M + m + 1] = as.integer(X >= xi[k + 1]) * (X - xi[k + 1])^m
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H = data.frame(H)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
H0 = matrix(0, nrow = n0, ncol = M*(K + 1) + 1)

# Global terms 
H0[,1] = 1
for(m in c(1:M)){
  H0[,m + 1] = x0^m
}

# Add a j^th order term after the i^th knot
# No 0^th order terms to ensure continuity
for(k in c(1:K)){
  for(m in c(1:M)){
    H0[, k*M + m + 1] = as.integer(x0 >= xi[k + 1]) * (x0 - xi[k + 1])^m
  }
} 

H0 = data.frame(H0)

y0 = predict(model, H0)

# Plot fitted model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)

# Plot fitted model
plot(X, Y, ylim = c(-3, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)




###
# Piecewise Cubic (smooth)
###

M = 3
K = 2

# Create basis expansion
H = matrix(0, nrow = n, ncol = (M - 1)*K + M + 1)

# Global terms 
H[,1] = 1
for(m in c(1:M)){
  H[,m + 1] = X^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 1))){
    H[, k*(M - 1) + m + 2] = as.integer(X >= xi[k + 1]) * (X - xi[k + 1])^(m + 1)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H = data.frame(H)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
# Create basis expansion
H0 = matrix(0, nrow = n0, ncol = (M - 1)*K + M + 1)

# Global terms 
H0[,1] = 1
for(m in c(1:M)){
  H0[,m + 1] = x0^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 1))){
    H0[, k*(M - 1) + m + 2] = as.integer(x0 >= xi[k + 1]) * (x0 - xi[k + 1])^(m + 1)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H0 = data.frame(H0)

y0 = predict(model, H0)

# Plot fitted model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)







###
# Cubic Spline
###

M = 3
K = 2

# Create basis expansion
H = matrix(0, nrow = n, ncol = (M - 2)*K + M + 1)

# Global terms 
H[,1] = 1
for(m in c(1:M)){
  H[,m + 1] = X^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 2))){
    H[, k + M + 1] = as.integer(X >= xi[k + 1]) * (X - xi[k + 1])^(m + 2)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H = data.frame(H)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
# Create basis expansion
H0 = matrix(0, nrow = n0, ncol = (M - 2)*K + M + 1)

# Global terms 
H0[,1] = 1
for(m in c(1:M)){
  H0[,m + 1] = x0^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 2))){
    H0[, k + M + 1] = as.integer(x0 >= xi[k + 1]) * (x0 - xi[k + 1])^(m + 2)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H0 = data.frame(H0)

y0 = predict(model, H0)

# Plot fitted model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)






###
# Cubic Spline (using bs)
###

require(splines)

M = 3
K = 2

# Create basis expansion
H = bs(X, knots = c(xi[2], xi[3]), degree = 3, intercept = TRUE)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
H0 = bs(x0, knots = c(xi[2], xi[3]), degree = 3, intercept = TRUE) 

y0 = predict(model, H0)

# Plot fitted model
lines(x0, y0, col='green', lwd=2, lty = 2)







###
# Cubic Spline (using bs)
###

require(splines)

M = 3
K = 2

# Create basis expansion
H = bs(X, knots = c(xi[2], xi[3]), Boundary.knots = c(0, 1), degree = 3, intercept = TRUE)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
H0 = bs(x0, knots = c(xi[2], xi[3]), Boundary.knots = c(0, 1), degree = 3, intercept = TRUE) 

y0 = predict(model, H0)

# Plot fitted model
#plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='blue', lwd=2, lty = 2)










###
# Cubic Spline
###

M = 3
K = 3

xi = c(-Inf, quantile(X, c(0.25, 0.5, 0.75)), Inf)

# Create basis expansion
H = matrix(0, nrow = n, ncol = (M - 2)*K + M + 1)

# Global terms 
H[,1] = 1
for(m in c(1:M)){
  H[,m + 1] = X^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 2))){
    H[, k + M + 1] = as.integer(X >= xi[k + 1]) * (X - xi[k + 1])^(m + 2)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H = data.frame(H)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
# Create basis expansion
H0 = matrix(0, nrow = n0, ncol = (M - 2)*K + M + 1)

# Global terms 
H0[,1] = 1
for(m in c(1:M)){
  H0[,m + 1] = x0^m
}

# Add a mth order term after the kth knot
# No 0th or 1st order terms to ensure smoothness
for(k in c(1:K)){
  for(m in c(1:(M - 2))){
    H0[, k + M + 1] = as.integer(x0 >= xi[k + 1]) * (x0 - xi[k + 1])^(m + 2)
  }
} 

# Convert to dataframe and name columns (for model fitting and plotting)
H0 = data.frame(H0)

y0 = predict(model, H0)

# Plot fitted model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)



###
# Cubic Spline (using bs)
###

require(splines)

# Create basis expansion
H = bs(X, df = K+M+1, Boundary.knots = c(0, 1), degree = M, intercept = TRUE)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
H0 = bs(x0, df = K + M + 1, Boundary.knots = c(0, 1), degree = M, intercept = TRUE) 

y0 = predict(model, H0)

# Plot fitted model
#plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='green', lwd=2, lty = 2)



###
# Cubic Spline (using bs)
###

require(splines)

# Create basis expansion
H = bs(X, df = K+M+1, Boundary.knots = c(0, 1), degree = M, intercept = TRUE)

# Fit using linear methods
model = lm(Y ~ 0 + ., data=H)

# Create basis for plotting
H0 = bs(x0, knots = attr(H, 'knots'), Boundary.knots = c(0, 1), degree = M, intercept = TRUE) 

y0 = predict(model, H0)

# Plot fitted model
#plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='blue', lwd=2, lty = 2)



###
# Concrete Stength - Spline Example with fixed df
###

mydata = read.csv('concrete-data.csv')
n = dim(mydata)[1]

# Define X and Y 
X = mydata$age
Y = mydata$strength

X = (X - mean(X)) / sd(X)

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Cement', bty = 'n')

df = 5

# For df = 5 we have several options:
# M = 1, K = 3
# M = 2, K = 2
# M = 3, K = 1
# M = 4, K = 0

mse = matrix(0, nrow = 4)
for(M in c(1:4)){
  H = bs(X, df = df, degree = M, intercept = TRUE)
  model = lm(Y ~ 0 + ., data=H)
  mse[M] = mean(residuals(model)^2)
}

which.min(mse)

M = 1
H = bs(X, df = df, degree = M, intercept = TRUE)
model = lm(Y ~ 0 + ., data=H)

x0 = seq(from = min(X), to = max(X), by = 0.01)
H0 = bs(x0, knots = attr(H, 'knots'), Boundary.knots = attr(H, 'Boundary.knots'), degree = M, intercept = TRUE) 
y0 = predict(model, H0)

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Cement', bty = 'n')
lines(x0, y0, lwd = 2, col = 'red')


###
# Concrete Stength - Spline Example with CV
###

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
    training_mse = matrix(0, nrow = df - 1)
    
    for(M in c(1:(df - 1))){
      H = bs(trainingX, df = df, degree = M, intercept = TRUE)
      model = lm(trainingY ~ 0 + ., data=H)
      training_mse[M] = mean(residuals(model)^2)
    }
    M = which.min(mse)
    H = bs(trainingX, df = df, degree = M, intercept = TRUE)
    model = lm(trainingY ~ 0 + ., data=H)
    
    testingH = bs(testingX, knots = attr(H, 'knots'), 
                  Boundary.knots = attr(H, 'Boundary.knots'), 
                  degree = M, intercept = TRUE) 
    testingYhat = predict(model, newdata = testingH)
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

mse = matrix(0, nrow = df - 1)
for(M in c(1:(df - 1))){
  H = bs(X, df = df, degree = M, intercept = TRUE)
  model = lm(Y ~ 0 + ., data=H)
  mse[M] = mean(residuals(model)^2)
}

which.min(mse)
M = which.min(mse)

H = bs(X, df = df, degree = M, intercept = TRUE)
model = lm(Y ~ 0 + ., data=H)

x0 = seq(from = min(X), to = max(X), by = 0.01)
H0 = bs(x0, knots = attr(H, 'knots'), Boundary.knots = attr(H, 'Boundary.knots'), degree = M, intercept = TRUE) 
y0 = predict(model, H0)

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Age', bty = 'n')
lines(x0, y0, lwd = 2, col = 'red')





###
# Concrete Stength - Spline Example with CV
###

mydata = read.csv('concrete-data.csv')
n = dim(mydata)[1]
mydata$age = mydata$age + rnorm(n, mean = 0, sd = 0.1)

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
    training_mse = matrix(0, nrow = df - 1)
    
    for(M in c(1:(df - 1))){
      H = bs(trainingX, df = df, degree = M, intercept = TRUE)
      model = lm(trainingY ~ 0 + ., data=H)
      training_mse[M] = mean(residuals(model)^2)
    }
    M = which.min(mse)
    H = bs(trainingX, df = df, degree = M, intercept = TRUE)
    model = lm(trainingY ~ 0 + ., data=H)
    
    testingH = bs(testingX, knots = attr(H, 'knots'), 
                  Boundary.knots = attr(H, 'Boundary.knots'), 
                  degree = M, intercept = TRUE) 
    testingYhat = predict(model, newdata = testingH)
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

mse = matrix(0, nrow = df - 1)
for(M in c(1:(df - 1))){
  H = bs(X, df = df, degree = M, intercept = TRUE)
  model = lm(Y ~ 0 + ., data=H)
  mse[M] = mean(residuals(model)^2)
}

which.min(mse)
M = which.min(mse)

H = bs(X, df = df, degree = M, intercept = TRUE)
model = lm(Y ~ 0 + ., data=H)

x0 = seq(from = min(X), to = max(X), by = 0.01)
H0 = bs(x0, knots = attr(H, 'knots'), Boundary.knots = attr(H, 'Boundary.knots'), degree = M, intercept = TRUE) 
y0 = predict(model, H0)

plot(X, Y, lwd = 2, ylab = 'strength', xlab = 'age', main = 'Scatterplot of Strength vs. Age', bty = 'n')
lines(x0, y0, lwd = 2, col = 'red')




####
# Natural Spline
#### 

# Load data
mydata = read.csv('piecewise-data.csv')

X = mydata$X
Y = mydata$Y
n = length(Y)

x0 = seq(from=0, to=1, by=0.01)
n0 = length(x0)

M = 3
K = 3

xi = c(quantile(X, c(0.25, 0.5, 0.75)))

# Fit a natural spline with knots at (1/3, 2/3)

# Create basis expansion
N = matrix(0, nrow = n, ncol = M)

# Global terms 
N[,1] = 1
N[,2] = X

# "d" terms
d = matrix(0, nrow = n, ncol = K - 1)
for(k in c(1:(K-1))){
  d[,k] = (as.integer(X - xi[k] > 0) * (X - xi[k])^3 - as.integer(X - xi[K] > 0) * (X - xi[K])^3) / (xi[K] - xi[k])
}

for(k in c(1:(K-2))){
  N[, k + 2] = d[,k] - d[,K-1]
}
N = as.data.frame(N)
model = lm(Y ~ 0 + ., data=N)


# Create basis expansion
N0 = matrix(0, nrow = n0, ncol = M)

# Global terms 
N0[,1] = 1
N0[,2] = x0

# "d" terms
d = matrix(0, nrow = n0, ncol = K - 1)
for(k in c(1:(K-1))){
  d[,k] = (as.integer(x0 - xi[k] > 0) * (x0 - xi[k])^3 - as.integer(x0 - xi[K] > 0) * (x0 - xi[K])^3) / (xi[K] - xi[k])
}

for(k in c(1:(K-2))){
  N0[, k + 2] = d[,k] - d[,K-1]
}
N0 = as.data.frame(N0)
y0 = predict(model, N0)

# Plot fitted model
plot(X, Y, ylim = c(-2, 2), bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example')
lines(x0, y0, col='red', lwd=2)

# Using the R function ns() to generate the basis (h vectors) of the spline
N = ns(X, knots = xi[2], Boundary.knots = c(xi[1], xi[3]), intercept=TRUE)
model = lm(Y ~ 0 + ., data=N)

# Default is to use the range of the inputs as the boundary knots
# We want to use the range of X (since x0 extends beyond X)
N0 = ns(x0, knots = attr(N, 'knots'), Boundary.knots = attr(N, 'Boundary.knots'), intercept=T)
y0 = predict(model, N0)

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


####
# Natural Spline (knots at every data point)
#### 

require(splines)

# Load data
mydata = read.csv('piecewise-data.csv')

X = mydata$X
Y = mydata$Y
n = length(Y)

x0 = seq(from=0, to=1, by=0.001)
n0 = length(x0)

# Fit a natural spline with knots at each data point
N = ns(X, df = 27, intercept = T)
model = lm(Y ~ 0 + ., data=N)

# Default is to use the range of the inputs as the boundary knots
# We want to use the range of X (since x0 extends beyond X)
N0 = ns(x0, knots = attr(N, 'knots'), Boundary.knots = attr(N, 'Boundary.knots'), intercept=T)
y0 = predict(model, N0)

# Plot fitted model
plot(X, Y, bty='n', xlab = 'X', ylab = 'Y', main = 'piecewise example', ylim = c(-2, 3))
lines(x0, y0, col='red', lwd=2)


attr(N, 'knots')
sort(X)

length(X)
length(attr(N, 'knots'))  


###
# Concrete Strength - Smoothing Spline Example  
###

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








############## Week 7 ALL ############################
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




###############################################################################
# SPLINE & KERNEL CHEAT SHEET (REFINED)
# References your posted code from Weeks 5–7
###############################################################################

############################# 1. SPLINE METHODS ################################
# We use the 'splines' package. Key functions:
#   - bs() for B-splines (piecewise polynomial)
#   - ns() for Natural Splines
#   - smooth.spline() for smoothing splines (can do CV automatically)
# Also see your custom piecewise expansions if you want manual basis code.

library(splines)

### Example: Cubic B-spline with specified knots
# Suppose we have data in 'mydata', with columns X, Y
x = mydata$X
y = mydata$Y
knot = c(0.25, 0.5, 0.75)        # internal knots
H = bs(x, knots = knot, degree = 3, intercept = TRUE)
model_cubic = lm(y ~ 0 + ., data=H)

# Predict & plot
x0 = seq(min(x), max(x), length.out = 200)
H0 = bs(x0, knots = knot, degree = 3, intercept = TRUE)
y0 = predict(model_cubic, newdata = H0)

plot(x, y)
lines(x0, y0, col='blue', lwd=2)

### Example: Natural Splines with df
df_val = 5
H_ns = ns(x, df = df_val, intercept=TRUE)
model_ns = lm(y ~ 0 + ., data=H_ns)

# Predict & plot
H0_ns = ns(x0, df = df_val, intercept=TRUE)
y0_ns = predict(model_ns, newdata=H0_ns)
plot(x, y)
lines(x0, y0_ns, col='red', lwd=2)

### Example: Smoothing Splines
# smooth.spline() can do GCV by default if you use 'cv = TRUE'.
# Manual lambda is also possible, e.g. lambda=0.01.

spline_fit = smooth.spline(x, y, cv = TRUE)   # picks df by GCV
optimal_df = spline_fit$df
plot(x, y)
lines(predict(spline_fit, x0), col='green', lwd=2)

### Cross-validation for Splines
# You already have code that sets up folds, loops over df (or knots),
# fits a B-spline/natural spline, & computes test MSE. 
# Example snippet (based on your doc):
# (1) Construct folds
# (2) For each df, fit 'lm(... ~ bs(...) or ns(...))'
# (3) Predict on hold-out data & track MSE
# (4) Pick df that yields min MSE

# See your "concrete-data.csv" example for the full loop.

###############################################################################
############################# 2. KERNEL METHODS ################################
# You posted many kernel methods, e.g. kernel_smoothing(), local_linear_regression()
# We’ll highlight the main ones plus typical usage.

### 2.1 Basic Kernels
uniform_kernel = function(t) {
  return(as.integer(abs(t) <= 1) / 2)
}

epanechnikov_kernel = function(t){
  return(as.integer(abs(t) <= 1) * (3/4) * (1 - t^2))
}

tricube_kernel = function(t){
  return(as.integer(abs(t) <= 1) * (1 - abs(t)^3)^3)
}

gaussian_kernel = function(t){
  return((2*pi)^(-1/2) * exp(-t^2 / 2))
}

### 2.2 Bandwidth Functions
constant_bandwidth = function(lambda, x0=NULL, X=NULL){
  return(lambda)
}

adaptive_bandwidth = function(lambda, x0, X){
  # 'lambda' is #neighbors or rank
  # returns distance to the 'lambda'th-nearest neighbor
  n = length(X)
  d = abs(X - x0)
  d_sorted = sort(d)
  return(d_sorted[lambda])
}

### 2.3 Kernel Smoother for Regression
# Uses weights from K(|x0 - X| / h), then does a weighted average
kernel_smoothing = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=0.25){
  w = kernel_fn(abs(x0 - X) / bandwidth_fn(lambda, x0, X))
  return(sum(w*Y) / sum(w))
}

### 2.4 Local Linear Regression
# Weighted least squares in the neighborhood around x0
local_linear_regression = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=2){
  n = length(Y)
  bw = bandwidth_fn(lambda, x0, X)
  w  = kernel_fn(abs(x0 - X)/bw)
  if(sum(w > 0) < 2) {
    stop('local regression requires at least 2 non-zero weights')
  }
  w = w / sum(w)
  W = diag(w)
  X_design = cbind(1, X)
  beta_hat = solve(t(X_design) %*% W %*% X_design) %*% t(X_design) %*% W %*% Y
  # Prediction at x0 => [1, x0] * beta_hat
  return(c(1, x0) %*% beta_hat)
}

### 2.5 Local Logistic Regression
# Weighted logistic with kernel-based weights
# Below is a simplified demonstration:
local_kernel_logistic = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=2){
  w = kernel_fn(abs(x0 - X)/bandwidth_fn(lambda, x0, X))
  w = w / sum(w)
  df_local = data.frame(X, Y)
  fit = glm(Y ~ X, data=df_local, family=binomial, weights=w)
  return(predict(fit, newdata=data.frame(X=x0), type='response'))
}

### 2.6 Example: Cross-Validate Kernel Bandwidth
# (You have code that loops over possible bandwidths or #neighbors,
# does a K-fold approach, and picks the one with min test MSE or max accuracy.)
# See your "constant_lambda_values" or "adaptive_lambda_values" examples.

###############################################################################
######################## 3. EXAMPLE USAGE (BRIEF) #############################
# Suppose we want to use kernel smoothing for the 'piecewise-data.csv'
# with an Epanechnikov kernel, constant bandwidth h=0.25:

mydata = read.csv('piecewise-data.csv')
X = mydata$X
Y = mydata$Y
x_grid = seq(min(X), max(X), length.out=200)

y_grid = numeric(length(x_grid))
for(i in seq_along(x_grid)) {
  y_grid[i] = kernel_smoothing(x_grid[i], X, Y, 
                               epanechnikov_kernel, 
                               constant_bandwidth, 
                               lambda=0.25)
}

plot(X, Y, main="Kernel Smoothing Example", bty='n')
lines(x_grid, y_grid, col='red', lwd=2)

###############################################################################
# SPLINE & KERNEL CHEAT SHEET WITH EXAMPLE USAGE
# (Adapted from your Weeks 5–7 code)
###############################################################################

library(splines)
# If you use glmnet or caret in examples, load them here:
# library(glmnet)
# library(caret)

###############################################################################
# 0. (Optional) Fold Maker for Cross-Validation
###############################################################################
make_folds = function(Y, nFolds, stratified = FALSE, seed = 0){
  # K-Fold cross validation
  # Input:
  #   Y (either sample size, or vector of outputs)
  #   stratified (boolean): whether the folds should 
  #     be stratified. If TRUE then Y should be a vector
  # Output: list of vectors of fold indices
  set.seed(seed)
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
        classfolds[[class]][[fold]] = shuffled_index[
          c((1 + (fold - 1) * n_per_fold) : (fold * n_per_fold))]
      }
      classfolds[[class]][[nFolds]] = shuffled_index[
        c(((nFolds - 1)*n_per_fold + 1) : n_class)]
    }
    folds = list()
    for(fold in 1:nFolds){
      folds[[fold]] = classfolds[[1]][[fold]]
      for(class in 2:nClasses){
        folds[[fold]] = c(folds[[fold]], classfolds[[class]][[fold]])
      }
    }
  } else {
    folds = list()
    n_per_fold = floor(n / nFolds)
    shuffled_index = sample(index)
    for(fold in c(1:(nFolds - 1))){
      folds[[fold]] = shuffled_index[
        c((1 + (fold - 1) * n_per_fold) : (fold * n_per_fold))]
    }  
    folds[[nFolds]] = shuffled_index[
      c(((nFolds - 1)*n_per_fold + 1) : n)]
  }
  return(folds)
}

###############################################################################
# 1. SPLINES
###############################################################################

# 1.1 B-splines with bs()
# Example usage:
# Suppose we have x, y from piecewise-data.csv
mydata = read.csv('piecewise-data.csv')
x = mydata$X
y = mydata$Y

# Sort x/y for a nicer plot
ord = order(x)
x = x[ord]
y = y[ord]

# Fit a cubic spline with knots
knots_vec = c(0.25, 0.5, 0.75)
H = bs(x, knots = knots_vec, degree = 3, intercept=TRUE)
model_bs = lm(y ~ 0 + ., data=H)  # no intercept because it's included in H

# Predict on a grid
x_grid = seq(0, 1, by=0.01)
H_grid = bs(x_grid, knots=knots_vec, degree=3, intercept=TRUE)
y_grid = predict(model_bs, newdata=H_grid)

plot(x, y, main='Cubic B-spline Example')
lines(x_grid, y_grid, col='blue', lwd=2)

# 1.2 Natural Splines with ns()
# You can specify df or knots. If you specify knots + df, the df includes boundary + intercept.
df_val = 5
H_ns = ns(x, df=df_val, intercept=TRUE)
model_ns = lm(y ~ 0 + ., data=H_ns)
H_grid_ns = ns(x_grid, df=df_val, intercept=TRUE)
y_grid_ns = predict(model_ns, newdata=H_grid_ns)

plot(x, y, main='Natural Spline Example')
lines(x_grid, y_grid_ns, col='red', lwd=2)

# 1.3 Smoothing Splines with smooth.spline()
# 'cv=TRUE' uses generalized cross-validation to pick df
fit_smooth = smooth.spline(x, y, cv=TRUE)
cat("Chosen df:", fit_smooth$df, "\n")

y_grid_smooth = predict(fit_smooth, x_grid)$y
plot(x, y, main='Smoothing Spline Fit')
lines(x_grid, y_grid_smooth, col='green', lwd=2)

# 1.4 Cross-validation for Splines (outline)
# Example: 10-fold CV over df in [2..20] for a natural spline
df_candidates = 2:20
nFolds = 5
folds = make_folds(length(y), nFolds)

mse_mat = matrix(0, nrow=length(df_candidates), ncol=nFolds)
for(k in 1:nFolds){
  test_idx = folds[[k]]
  train_idx = setdiff(1:length(y), test_idx)
  
  x_train = x[train_idx]
  y_train = y[train_idx]
  x_test  = x[test_idx]
  y_test  = y[test_idx]
  
  # (Optional: standardize x here if needed)
  for(i in seq_along(df_candidates)){
    df_i = df_candidates[i]
    # Fit on training
    train_ns = ns(x_train, df=df_i, intercept=TRUE)
    model_tmp = lm(y_train ~ 0 + ., data=train_ns)
    # Predict on test
    test_ns = ns(x_test, knots=attr(train_ns,"knots"),
                 Boundary.knots=attr(train_ns,"Boundary.knots"),
                 intercept=TRUE)
    yhat_test = predict(model_tmp, newdata=test_ns)
    mse_mat[i,k] = mean((y_test - yhat_test)^2)
  }
}
mse_vec = rowMeans(mse_mat)
best_df = df_candidates[which.min(mse_vec)]
cat("Best df for NS via CV:", best_df, "with MSE=", min(mse_vec), "\n")

###############################################################################
# 2. KERNEL METHODS
###############################################################################

# 2.1 Basic Kernels
uniform_kernel = function(t){
  as.integer(abs(t) <= 1) / 2
}
epanechnikov_kernel = function(t){
  as.integer(abs(t) <= 1) * (3/4)*(1 - t^2)
}
tricube_kernel = function(t){
  as.integer(abs(t) <= 1) * (1 - abs(t)^3)^3
}
gaussian_kernel = function(t){
  (1/sqrt(2*pi)) * exp(-t^2 / 2)
}

# 2.2 Bandwidth functions
constant_bandwidth = function(lambda, x0=NULL, X=NULL){
  return(lambda)
}
adaptive_bandwidth = function(lambda, x0, X){
  d = abs(X - x0)
  d_sorted = sort(d)
  return(d_sorted[lambda])  # e.g. the lambda'th NN distance
}

# 2.3 Kernel Smoothing (Regression)
kernel_smoothing = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=0.25){
  w = kernel_fn(abs(x0 - X) / bandwidth_fn(lambda, x0, X))
  return(sum(w * Y) / sum(w))
}

# Quick example usage: (Epanechnikov, constant h=0.2)
plot(x, y, main='Kernel Smoothing Example', bty='n')
x_grid_k = seq(0, 1, by=0.01)
y_grid_k = numeric(length(x_grid_k))
for(i in seq_along(x_grid_k)){
  y_grid_k[i] = kernel_smoothing(x_grid_k[i], x, y, 
                                 epanechnikov_kernel, 
                                 constant_bandwidth,
                                 lambda=0.2)
}
lines(x_grid_k, y_grid_k, col='purple', lwd=2)

# 2.4 Local Linear Regression
local_linear_regression = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=2){
  n = length(Y)
  h = bandwidth_fn(lambda, x0, X)
  w = kernel_fn(abs(x0 - X)/h)
  # Weighted design matrix
  if(sum(w>0) < 2) stop("Not enough points with non-zero weight")
  w = w / sum(w)
  W = diag(w)
  Xd = cbind(1, X)
  beta_hat = solve(t(Xd) %*% W %*% Xd) %*% t(Xd) %*% W %*% Y
  return(c(1, x0) %*% beta_hat)
}

# Example usage: local linear with epanechnikov, h=0.25
y_grid_ll = numeric(length(x_grid_k))
for(i in seq_along(x_grid_k)){
  y_grid_ll[i] = local_linear_regression(x_grid_k[i], x, y,
                                         epanechnikov_kernel, 
                                         constant_bandwidth,
                                         lambda=0.25)
}
plot(x, y, main='Local Linear Regression Example', bty='n')
lines(x_grid_k, y_grid_ll, col='red', lwd=2)

# 2.5 Local Logistic Regression
local_kernel_logistic = function(x0, X, Y, kernel_fn, bandwidth_fn, lambda=1){
  # Weighted logistic regression
  w = kernel_fn(abs(x0 - X) / bandwidth_fn(lambda, x0, X))
  w = w / sum(w)
  df_local = data.frame(X=X, Y=Y)
  fit_loclogit = glm(Y ~ X, data=df_local, family=binomial, weights=w)
  prob_x0 = predict(fit_loclogit, newdata=data.frame(X=x0), type='response')
  return(prob_x0) # Probability of Y=1 at x0
}

# Example usage: logistic classification on a simple dataset
# Suppose we have mydata with X=some numeric predictor, Y in {0,1}
# x0=0.4 => local logistic prob:
# local_prob = local_kernel_logistic(0.4, mydata$X, mydata$Y,
#                                    epanechnikov_kernel,
#                                    constant_bandwidth, lambda=0.2)

# 2.6 Cross-Validation for Kernel Bandwidth
# Outline: loop over candidate bandwidths, do K-fold, compute MSE or classification error
bandwidth_candidates = seq(0.05, 0.5, by=0.05)
mse_kern = matrix(0, nrow=length(bandwidth_candidates), ncol=nFolds)
for(k in 1:nFolds){
  test_idx = folds[[k]]
  train_idx = setdiff(1:length(y), test_idx)
  x_train = x[train_idx]
  y_train = y[train_idx]
  x_test  = x[test_idx]
  y_test  = y[test_idx]
  
  for(i in seq_along(bandwidth_candidates)){
    h_i = bandwidth_candidates[i]
    pred_test = numeric(length(x_test))
    for(j in seq_along(x_test)){
      pred_test[j] = kernel_smoothing(x_test[j], x_train, y_train,
                                      gaussian_kernel, 
                                      constant_bandwidth,
                                      lambda=h_i)
    }
    mse_kern[i,k] = mean((y_test - pred_test)^2)
  }
}
mse_kern_mean = rowMeans(mse_kern)
best_h = bandwidth_candidates[which.min(mse_kern_mean)]
cat("Best bandwidth for kernel smoothing:", best_h, "\n")

###############################################################################
# 3. SUMMARY OF USAGE
###############################################################################
# SPLINE:
#   - Use 'bs(...)' or 'ns(...)' to build design matrix -> lm() or glm().
#   - 'smooth.spline(x, y, cv=TRUE)' for automatic smoothing spline.
#   - Cross-validation loops for df or knots as needed.
#
# KERNEL:
#   - Provide a kernel function (Epanechnikov, Gaussian, etc.).
#   - Provide a bandwidth function (constant or adaptive).
#   - 'kernel_smoothing(x0, X, Y, kernel_fn, bandwidth_fn, lambda)' for 1D regression.
#   - 'local_linear_regression(x0, ...)' for local linear fit.
#   - 'local_kernel_logistic(x0, ...)' for local logistic classification.
#   - K-fold CV to pick bandwidth or #neighbors.
#
###############################################################################

###############################################################################
# WEEK 5: MODEL SELECTION CHEAT SHEET (IN R COMMENTS)
###############################################################################

# 1. OVERFITTING & BIAS-VARIANCE TRADEOFF
# -----------------------------------------------------------------------------
# - Overfitting: model memorizes noise in training data ⇒ poor generalization.
# - Bias-Variance Tradeoff:
#     * High complexity => low bias, high variance
#     * Low complexity  => high bias, low variance
# - Often plotted as training error vs. testing error vs. model complexity.

###############################################################################

# 2. MODEL COMPLEXITY & DEGREES OF FREEDOM
# -----------------------------------------------------------------------------
# - Model Complexity: 
#     * #parameters in linear models, 
#     * #knots (or df) in splines, 
#     * penalty λ in LASSO/Ridge,
#     * bandwidth or k in kNN.
#
# - Degrees of Freedom (df):
#     * Linear reg w/ p inputs => df = p + 1 (including intercept).
#     * Ridge => df = trace(X (X'X + λI)^(-1) X').
#     * LASSO => df depends on how many coefficients are nonzero (depends on λ).
#     * Smoothing spline => directly set by the 'df' parameter (more df => more wiggle).

###############################################################################

# 3. MODEL SELECTION & ASSESSMENT
# -----------------------------------------------------------------------------
# - Train vs. Validate vs. Test:
#     * Train model on training set.
#     * Validate to pick hyperparameters (e.g., λ for LASSO).
#     * (Ideally) final test set to assess performance.
# - If not enough data, often rely on cross-validation (K-fold).
#
# - AIC:
#     * "Akaike Information Criterion" ≈ training error + 2*(#params).
#     * Lower AIC => better in-sample fit (with a complexity penalty).

###############################################################################

# 4. CROSS-VALIDATION
# -----------------------------------------------------------------------------
# - K-Fold CV:
#     * Partition data into K roughly equal parts.
#     * For each k in 1..K: 
#         => train on T(-k), validate on T(k).
#         => compute validation error.
#     * Average across folds => estimate of unconditional expected error.
#
# - LOOCV (K=n):
#     * Minimizes bias, but can have high variance in error estimate.
#
# - Stratified K-Fold: 
#     * For classification, ensures class proportions are consistent in each fold.

###############################################################################

# 5. LOCAL METHODS & KNN
# -----------------------------------------------------------------------------
# - kNN classification/regression => hyperparameter k (#neighbors).
#     * Smaller k => more complex decision boundary (less bias, more variance).
# - Kernel smoothing => bandwidth = complexity parameter.
#     * smaller bandwidth => more local fit => can overfit.
# - Local linear regression => weighted least squares around each x0.

###############################################################################

# 6. COMMON PITFALLS & NOTES
# -----------------------------------------------------------------------------
# - "Scaling inputs" typically done before splitting (or carefully in CV).
# - Overreliance on training error => overfitting.
# - Degrees of freedom for LASSO or spline are not just the # of columns in data.
# - Use CV or test set to pick hyperparameters, then refit the final model on the full data.

###############################################################################

# 7. R CODING SNIPPETS (BRIEF)
# -----------------------------------------------------------------------------
# (a) B-Spline / Natural Spline:
#    library(splines)
#    # Suppose x, y are vectors
#    fit_bs <- lm(y ~ bs(x, df=5))               # B-spline
#    fit_ns <- lm(y ~ ns(x, df=5))               # Natural spline
#
# (b) Smoothing Spline:
#    fit_smooth <- smooth.spline(x, y, df=5)
#    # or
#    fit_smooth_gcv <- smooth.spline(x, y, cv=TRUE)
#
# (c) Cross-Validation Loop (Pseudo):
#    folds <- sample(rep(1:K, length.out = length(y)))
#    mse_fold <- numeric(K)
#    for(k in 1:K){
#      train_idx <- which(folds != k)
#      test_idx  <- which(folds == k)
#      # Fit model on train_idx, predict on test_idx
#      # Compute error => store in mse_fold[k]
#    }
#    cv_error <- mean(mse_fold)
#
# (d) Kernel Smoothing:
#    # built-in "ksmooth" for 1D
#    result <- ksmooth(x, y, "normal", bandwidth=0.5, x.points=new_x)
#    # or custom local_linear_regression
#
# (e) Local Logistic:
#    # Weighted glm with family="binomial"
#    fit <- glm(y ~ x, family=binomial, weights=w)
#
###############################################################################






