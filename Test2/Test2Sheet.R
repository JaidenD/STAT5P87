data = read.csv('piecewise-data.csv')
x = data$X
y = data$Y
# spline methods

# cubic spline-------------------------------------------------
ordr = order(x)
x = x[ordr]
y = y[ordr]

knot = c(0.25, 0.5, 0.75) # example
H = bs(X, knots = knot, degree = 3, intercept = TRUE)
model = lm(Y ~ 0 + ., data=H)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)

H0 = bs(x0, knots = knot, degree = 3, intercept = TRUE) 

y0 = predict(model, H0)
plot(x,y)
lines(x0, y0, col='green', lwd=2, lty = 2)

# natural spline--------------------------------------------
ordr = order(x)
x = x[ordr]
y = y[ordr]

knot = c(0.25, 0.5, 0.75) # example
H = ns(x,df=3, knots = knot, intercept=TRUE)
model = lm(Y ~ 0 + ., data=H)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)
H0 = ns(x0,df=3, knots = knot, intercept=TRUE)

y0 = predict(model, H0)
plot(x,y)
lines(x0, y0, col='green', lwd=2, lty = 2)

# smoothing spline (not correct).
ordr = order(x)
x = x[ordr]
y = y[ordr]

knot = unique(x) # example
H = ns(x,df=3, knots = knot, intercept=TRUE)
model = lm(Y ~ 0 + ., data=H)

# For plotting smoothness
x0 = seq(from=0, to=1, by=0.01)
H0 = ns(x0,df=3, knots = knot, intercept=TRUE)

y0 = predict(model, H0)
plot(x,y)
lines(x0, y0, col='green', lwd=2, lty = 2)

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

