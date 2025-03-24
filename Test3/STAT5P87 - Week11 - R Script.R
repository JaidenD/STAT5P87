### STAT5P87 - Week11.R ###

make_folds = function(Y, nFolds, stratified = FALSE, seed = 0){
  # K-Fold cross validation
  # Input:
  #   Y (either sample size, or vector of outputs)
  #   stratified (boolean): whether the folds should 
  #     be stratified. Requires Y to be a vector of outputs
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



### Separable Example

mydata = read.csv('separable-data.csv')

x1range = c(min(mydata$x1), max(mydata$x1))
x2range = c(min(mydata$x2), max(mydata$x2))

plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 3, xlab = 'x1', ylab = 'x2', cex = 1.5)

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'blue', 
       cex = 1.5, lwd = 3)


# make up a few lines that separate the data

lines(c(-1, 2), c(0.5, 0.5))
lines(c(-1, 2), c(0.7, 0.7))
lines(c(-1, 2), c(0.4, 0.4))

# 1 - 0.5x2

lines(c(-1, 2), c(1 - 0.5*(-1), 1 - 0.5*2))

lines(c(-1, 2), c(1.1 - 0.6*(-1), 1.1 - 0.6*2))








### Solve the problem using e1071

require(e1071)

# kernel = linear to make the SVM and SVC
# cost = 1e100 makes the SVC return the seperable hyperplane solution
fit = svm(y ~ x1 + x2, data=mydata, scale = FALSE, type = 'C',
          kernel = 'linear', cost = 1e100)

# fit$coefs is the value of y_i * alpha_i for all alpha_i != 0
fit$coefs
sum(fit$coefs)

# which data points are support vectors
fit$index


fit$coefs / mydata$y[fit$index]

# note that the alpha/y term is the wrong sign...
# this is because, for some reason, svm() sometimes reverses the sign of 
# the outputs.
fit$coefs / (-mydata$y[fit$index])

# Support Vectors
fit$SV

# Estimate B
bHat <- t(fit$coefs) %*% fit$SV

# Solve for b0 using SV[1]
x = as.matrix(c(mydata$x1[fit$index[1]], mydata$x2[fit$index[1]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[1]]) / fit$coefs[1]
b0

# Solve for b0 using SV[2]
x = as.matrix(c(mydata$x1[fit$index[2]], mydata$x2[fit$index[2]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[2]]) / fit$coefs[1]
b0

# Solve for b0 using SV[3]
x = as.matrix(c(mydata$x1[fit$index[3]], mydata$x2[fit$index[3]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[3]]) / fit$coefs[1]
b0


# f(x) = b0 + xB = b0 + b1x1 + b2x2 = 0
# b0 + b1x1 + b2x2 = 0
# x2 = -(b1x1 + b0)/b2

plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 2, xlab = 'x1', ylab = 'x2')

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'blue', lwd = 2)

points(mydata[fit$index,c(2,3)],col='black',cex=3, lwd = 2)

abline(a=-b0/bHat[2], b=-bHat[1]/bHat[2], col="black", lwd = 2)










### 
# Simple Classification
###

mydata = read.csv('simple-classification-data.csv')
mydata$y[mydata$y == 0] = -1

n = dim(mydata)[1]
nFolds = 5

folds = make_folds(n, nFolds, F, seed = 0)

C_values = seq(from = 0.02, to = 3, by = 0.02)
n_C_values = length(C_values)

accuracy = matrix(0, nrow = n_C_values, ncol = nFolds)

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  for(i in 1:n_C_values){
    fit = svm(y ~ x1 + x2, data=training_data, scale = FALSE, type = 'C',
          kernel = 'linear', cost = C_values[i])
    
    yHat = predict(fit, newdata = testing_data)
    accuracy[i, fold] = mean(yHat == testing_data$y)
  }
}

accuracy = apply(accuracy, 1, mean)

plot(C_values, accuracy)

C = C_values[which.max(accuracy)]
C
max(accuracy)


fit = svm(y ~ x1 + x2, data=mydata, scale = FALSE, type = 'C',
          kernel = 'linear', cost = C)

fit$coefs / (-mydata$y[fit$index])

bHat <- t(fit$coefs) %*% fit$SV
b0 = -fit$rho

x1range = c(min(mydata$x1), max(mydata$x1))
x2range = c(min(mydata$x2), max(mydata$x2))

plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 2, xlab = 'x1', ylab = 'x2')

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'blue', lwd = 2)
points(mydata[fit$index,c(2,3)], col='black', cex=5, pch = '.', lwd = 2)

abline(a=-b0/bHat[2], b=-bHat[1]/bHat[2], col="black", lwd = 2)






### 
# Simple Classification - Expanded
###

mydata = read.csv('simple-classification-data.csv')
mydata$y[mydata$y == 0] = -1

n = dim(mydata)[1]
nFolds = 5

folds = make_folds(n, nFolds, F, seed = 0)

C_values = seq(from = 0.05, to = 3, by = 0.05)
n_C_values = length(C_values)

accuracy = matrix(0, nrow = n_C_values, ncol = nFolds)

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  for(i in 1:n_C_values){
    fit = svm(y ~ x1 * x2, data=training_data, scale = FALSE, type = 'C',
              kernel = 'linear', cost = C_values[i])
    
    yHat = predict(fit, newdata = testing_data)
    accuracy[i, fold] = mean(yHat == testing_data$y)
  }
}

accuracy = apply(accuracy, 1, mean)

plot(C_values, accuracy)

C = C_values[which.max(accuracy)]
C

max(accuracy)


fit = svm(y ~ x1 * x2, data=mydata, scale = FALSE, type = 'C',
          kernel = 'linear', cost = C)

bHat <- t(fit$coefs) %*% fit$SV
b0 = -fit$rho

x1range = c(min(mydata$x1), max(mydata$x1))
x2range = c(min(mydata$x2), max(mydata$x2))

plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 2, xlab = 'x1', ylab = 'x2')

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'blue', lwd = 2)
points(mydata[fit$index,c(2,3)], col='black', cex=5, pch = '.', lwd = 2)

# f(x) = b0 + x1b1 + x2b2 + x1x2b3 = 0

x1 = seq(from = min(mydata$x1), to = 1.85, length.out = 1000)
x2 = (-b0 - bHat[1]*x1) / (bHat[2] + x1*bHat[3])

lines(x1, x2, lwd = 2)



yHat = predict(fit, newdata=mydata)

plot(mydata$x1[yHat == -1], mydata$x2[yHat == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 2, xlab = 'x1', ylab = 'x2')

points(mydata$x1[yHat == 1], mydata$x2[yHat == 1], col = 'blue', lwd = 2)
points(mydata[fit$index,c(2,3)],col='black',cex=3, lwd = 2)

# f(x) = b0 + x1b1 + x2b2 + x1x2b3 = 0

x1 = seq(from = min(mydata$x1), to = 1.85, length.out = 1000)
x2 = (-b0 - bHat[1]*x1) / (bHat[2] + x1*bHat[3])

lines(x1, x2)




### 
# Simple Classification - Expanded
###

mydata = read.csv('simple-classification-data.csv')
mydata$y[mydata$y == 0] = -1

n = dim(mydata)[1]
nFolds = 5

folds = make_folds(n, nFolds, F, seed = 0)

C_values = seq(from = 2.05, to = 5, by = 0.05)
n_C_values = length(C_values)

gamma_values = seq(from = 2.05, to = 3, by = 0.05)
n_gamma_values = length(gamma_values)

accuracy = array(0, dim = c(n_C_values, n_gamma_values, nFolds))

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  for(i in 1:n_C_values){
    for(j in 1:n_gamma_values){
      fit = svm(y ~ x1 + x2, data=training_data, scale = FALSE, type = 'C',
              kernel = 'radial', cost = C_values[i], gamma = gamma_values[j])
      yHat = predict(fit, newdata = testing_data)
      accuracy[i, j, fold] = mean(yHat == testing_data$y)
    }
  }
}

accuracy = apply(accuracy, c(1,2), mean)

which(accuracy == max(accuracy), arr.ind = T)

C = C_values[36]
gamma = gamma_values[6]

max(accuracy)

fit = svm(y ~ ., data=mydata, scale = FALSE, type = 'C',
          kernel = 'radial', cost = C, gamma = gamma)

# Visualize the decision boundary

n = dim(mydata)[1]
x_grid = seq(from=-1.75, to=2.75, by=0.05)
y_grid = seq(from=-1.75, to=2.75, by=0.05)

n_grid_points = length(x_grid)

grid = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)

for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    grid[i, j] = predict(fit, newdata = matrix(c(x_grid[i], y_grid[j]), nrow = 1))
  }
}

blue_index = which(grid == 1, arr.ind=TRUE)
orange_index = which(grid == 2, arr.ind=TRUE)

# colour-coded plot
plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlab = 'x1', ylab = 'x2', 
     bty = 'n', col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))
points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], xlab = 'x1', ylab = 'x2', 
       bty = 'n', col = 'orange', lwd = 2)

points(x_grid[blue_index[,1]], y_grid[blue_index[,2]], cex=2, pch='.', col='blue')
points(x_grid[orange_index[,1]], y_grid[orange_index[,2]], cex=2, pch='.', col='orange')



###
# SA Heart Example
###

require(e1071)

mydata = read.csv('SAHeart-data.csv')
mydata$chd[mydata$chd == 0] = -1

mydata = mydata[, c(2, 9, 10)]

n = dim(mydata)[1]
nFolds = 5

folds = make_folds(mydata$chd, nFolds, T, seed = 0)

C_values = seq(from = 0.1, to = 0.5, by = 0.05)
n_C_values = length(C_values)

gamma_values = seq(from = 0.005, to = 0.05, by = 0.005)
n_gamma_values = length(gamma_values)

accuracy = array(0, dim = c(n_C_values, n_gamma_values, nFolds))

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  for(i in 1:n_C_values){
    for(j in 1:n_gamma_values){
      fit = svm(chd ~ ., data=training_data, scale = FALSE, type = 'C',
              kernel = 'radial', cost = C_values[i], gamma = gamma_values[j])
      yHat = predict(fit, newdata = testing_data)
      accuracy[i, j, fold] = mean(yHat == testing_data$chd)
    }
  }
}

accuracy = apply(accuracy, c(1,2), mean)

which(accuracy == max(accuracy), arr.ind = T)

plot(C_values, accuracy[,4])
plot(gamma_values, accuracy[4,])

C = C_values[4]
gamma = gamma_values[4]

max(accuracy)


# Visualize the decision boundary

fit = svm(chd ~ ., data=mydata, scale = FALSE, type = 'C',
          kernel = 'radial', cost = C, gamma = gamma)

n = dim(mydata)[1]

x_min = min(mydata$tobacco)
x_max = max(mydata$tobacco)

y_min = min(mydata$age)
y_max = max(mydata$age)

x_grid = seq(from=x_min, to=x_max, length.out=50)
y_grid = seq(from=y_min, to=y_max, length.out=50)

n_grid_points = length(x_grid)

grid = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)

for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    grid[i, j] = predict(fit, newdata = matrix(c(x_grid[i], y_grid[j]), nrow = 1))
  }
}

blue_index = which(grid == 1, arr.ind=TRUE)
orange_index = which(grid == 2, arr.ind=TRUE)

# colour-coded plot
plot(mydata$tobacco[mydata$chd == -1], mydata$age[mydata$chd == -1], xlab = 'tobacco', ylab = 'age', 
     bty = 'n', col = 'blue', lwd = 2, xlim = c(x_min, x_max),
     ylim = c(y_min, y_max))
points(mydata$tobacco[mydata$chd == 1], mydata$age[mydata$chd == 1], bty = 'n', col = 'orange', lwd = 2)

points(x_grid[blue_index[,1]], y_grid[blue_index[,2]], cex=2, pch='.', col='blue')
points(x_grid[orange_index[,1]], y_grid[orange_index[,2]], cex=2, pch='.', col='orange')





###
# SA Heart Example
###

require(e1071)

mydata = read.csv('SAHeart-data.csv')
mydata$chd[mydata$chd == 0] = -1

n = dim(mydata)[1]
nFolds = 5

folds = make_folds(mydata$chd, nFolds, T, seed = 0)

C_values = seq(from = 0.2, to = 4, by = 0.2)
n_C_values = length(C_values)

gamma_values = seq(from = 0.004, to = 0.05, by = 0.002)
n_gamma_values = length(gamma_values)

accuracy = array(0, dim = c(n_C_values, n_gamma_values, nFolds))

for(fold in 1:nFolds){
  
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  for(i in 1:n_C_values){
    for(j in 1:n_gamma_values){
      fit = svm(chd ~ ., data=training_data, scale = TRUE, type = 'C',
                kernel = 'radial', cost = C_values[i], gamma = gamma_values[j])
      yHat = predict(fit, newdata = testing_data)
      accuracy[i, j, fold] = mean(yHat == testing_data$chd)
    }
  }
}

accuracy = apply(accuracy, c(1,2), mean)

which(accuracy == max(accuracy), arr.ind = T)

plot(C_values, accuracy[,4])
plot(gamma_values, accuracy[17,])

C = C_values[17]
gamma = gamma_values[4]

max(accuracy)



