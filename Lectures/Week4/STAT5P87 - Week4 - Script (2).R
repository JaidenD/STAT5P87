### STAT5P87 - Week 4 R Script



### 
# Masking Example
###

mydata = read.csv('masking-example.csv')
head(mydata)

n = dim(mydata)[1]

Y = as.matrix(mydata$Y, nrow = n)
X = model.matrix(Y ~ ., data=mydata)
p = dim(X)[2] - 1

x1min = min(X[,2])
x1max = max(X[,2])

x2min = min(X[,3])
x2max = max(X[,3])

plot(X[Y == 1, 2], X[Y == 1, 3], col = 'forestgreen', pch = 2, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(X[Y == 2, 2], X[Y == 2, 3], col = 'darkorange1', pch = 2, lwd=2)
points(X[Y == 3, 2], X[Y == 3, 3], col = 'red', pch = 2, lwd=2)

legend(-0.2, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)


# Define indicator variables for each class, Y1, Y2, Y3

Y1 = ifelse(Y == 1, 1, 0)
Y2 = ifelse(Y == 2, 1, 0)
Y3 = ifelse(Y == 3, 1, 0)
allY = cbind(Y1, Y2, Y3)

BHat = solve(t(X) %*% X) %*% t(X) %*% allY

x1_grid = seq(from=x1min, to=x1max, length = 150)
x2_grid = seq(from=x2min, to=x2max, length = 150)

n_grid_points = length(x1_grid)

grid_linear = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)

for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    x = matrix(c(1, x1_grid[i], x2_grid[j]), nrow = 1)
    yHat = x %*% BHat
    grid_linear[i, j] = which.max(yHat)
  }
}

red_index = which(grid_linear == 3, arr.ind=TRUE)
orange_index = which(grid_linear == 2, arr.ind=TRUE)
green_index = which(grid_linear == 1, arr.ind=TRUE)

# colour-coded plot

plot(X[Y == 1, 2], X[Y == 1, 3], col = 'forestgreen', pch = 2, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(X[Y == 2, 2], X[Y == 2, 3], col = 'darkorange1', pch = 2, lwd=2)
points(X[Y == 3, 2], X[Y == 3, 3], col = 'red', pch = 2, lwd=2)

legend(-0.2, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)

points(x1_grid[red_index[,1]], x2_grid[red_index[,2]], cex=2, pch='.', col='red')
points(x1_grid[orange_index[,1]], x2_grid[orange_index[,2]], cex=2, pch='.', col='darkorange1')
points(x1_grid[green_index[,1]], x2_grid[green_index[,2]], cex=2, pch='.', col='forestgreen')



## Masking using logistic regression

# using the factor Y as input
require(glmnet)

Y = factor(Y)
K = length(levels(Y))
classes = levels(Y)

fit = glmnet(X, Y, family = 'multinomial', lambda = 0)
yHat = predict(fit, newx = X, type = 'class')
mean(yHat == Y)

grid_logistic = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)

for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    grid_logistic[i, j] = predict(fit, newx = matrix(c(1, x1_grid[i], x2_grid[j]), nrow = 1), type = 'class')
  }
}

red_index = which(grid_logistic == 3, arr.ind=TRUE)
orange_index = which(grid_logistic == 2, arr.ind=TRUE)
green_index = which(grid_logistic == 1, arr.ind=TRUE)

plot(X[Y == 1, 2], X[Y == 1, 3], col = 'forestgreen', pch = 2, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(X[Y == 2, 2], X[Y == 2, 3], col = 'darkorange1', pch = 2, lwd=2)
points(X[Y == 3, 2], X[Y == 3, 3], col = 'red', pch = 2, lwd=2)

legend(-0.2, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)


# colour-coded plot
points(x1_grid[red_index[,1]], x2_grid[red_index[,2]], cex=2, pch='.', col='red')
points(x1_grid[orange_index[,1]], x2_grid[orange_index[,2]], cex=2, pch='.', col='darkorange1')
points(x1_grid[green_index[,1]], x2_grid[green_index[,2]], cex=2, pch='.', col='forestgreen')


### Linear Discriminant Analysis

training_data = read.csv('vowel_training.csv')
testing_data = read.csv('vowel_testing.csv')

Y = factor(training_data$y)
X = model.matrix(y ~ 0 + ., data=training_data)
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

# Estimate covariance matrix
SigmaHat = matrix(0, nrow = p, ncol = p)
for(k in 1:K){
    muMatrix = matrix(rep(muHat[k,], nK[k]), 
                      nrow = nK[k], ncol = p,
                      byrow = TRUE)
    SigmaHat = SigmaHat + t(X[Y == classes[k],] - muMatrix) %*% (X[Y == classes[k],] - muMatrix)
}
SigmaHat = SigmaHat / (n - K)

# Compute discriminants
invSigma = solve(SigmaHat)
delta = matrix(NA, nrow = n, ncol = K)
for(i in 1:n){
  for(k in 1:K){
    delta[i, k] = qK[k] * exp(X[i,] %*% invSigma %*% muHat[k,] - (1/2)*muHat[k,]%*%invSigma%*%muHat[k,])
  }
}

# Predict observation 1
classes[which.max(delta[1,])]

# Predict all observation
yHat = apply(delta, 1, which.max)

# Compute training accuracy
mean(yHat == Y)


### Testing Accuracy

# Load testing data
testing_data = read.csv('vowel_testing.csv')
Y_testing = factor(testing_data$y)
X_testing = model.matrix(y ~ 0 + ., data=testing_data)

# Compute discriminant for each observation / class
n_testing = length(Y_testing)
delta_testing = matrix(NA, nrow = n_testing, ncol = K)
for(i in 1:n_testing){
  for(k in 1:K){
    delta_testing[i, k] = qK[k] * exp(X_testing[i,] %*% invSigma %*% muHat[k,] - (1/2)*muHat[k,]%*%invSigma%*%muHat[k,])
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


### Regularized LDA
# SigmaHat, muHat, and qK estimated above

# Normalize the data according to the pooled variance
normalizing_vector = diag(SigmaHat)
normalizing_matrix = matrix(rep(normalizing_vector, n), nrow = n, byrow = TRUE)
dim(normalizing_matrix)
dim(X)
X = X / sqrt(normalizing_matrix)

# Re-estimate mu and Sigma

# Estimate a mean vector muHat_k for each class
muHat = matrix(NA, ncol = p, nrow = K)
for(k in 1:K){
  muHat[k,] = apply(X[Y == classes[k],], 2, mean)
}

# Estimate covariance matrix
SigmaHat = matrix(0, nrow = p, ncol = p)
for(k in 1:K){
  muMatrix = matrix(rep(muHat[k,], nK[k]), 
                    nrow = nK[k], ncol = p,
                    byrow = TRUE)
  SigmaHat = SigmaHat + t(X[Y == classes[k],] - muMatrix) %*% (X[Y == classes[k],] - muMatrix)
}
SigmaHat = SigmaHat / (n - K)

# Check if normalization is successful
diag(SigmaHat)

# Apply with alpha = 0.5
alpha = 0.5
SigmaHat_alpha = (1 - alpha) * SigmaHat + alpha * diag(p)


# Load testing data
testing_data = read.csv('vowel_testing.csv')
Y_testing = factor(testing_data$y)
X_testing = model.matrix(y ~ 0 + ., data=testing_data)
n_testing = length(Y_testing)

normalizing_matrix = matrix(rep(normalizing_vector, n_testing), nrow = n_testing, byrow = TRUE)
X_testing = X_testing / sqrt(normalizing_matrix)

# Compute discriminant for each observation / class
delta_testing = matrix(NA, nrow = n_testing, ncol = K)

# Pre-compute Sigma_alpha inverse
invSigma_alpha = solve(SigmaHat_alpha)
for(i in 1:n_testing){
  for(k in 1:K){
    delta_testing[i, k] = qK[k] * exp(X_testing[i,] %*% invSigma_alpha %*% muHat[k,] - (1/2)*muHat[k,] %*% invSigma_alpha %*% muHat[k,])
  }
}

# Find class that maximizes discriminant
yHat_testing = classes[apply(delta_testing, 1, which.max)]

# Compute testing accuracy
mean(yHat_testing == Y_testing)



### Try a grid of alpha values

alpha_values = seq(from = 0, to = 1, by = 0.01)
n_alpha_values = length(alpha_values)
accuracy = matrix(NA, nrow = n_alpha_values)

for(j in 1:n_alpha_values){
  alpha = alpha_values[j]
  SigmaHat_alpha = (1 - alpha) * SigmaHat + alpha * diag(p)
  invSigma_alpha = solve(SigmaHat_alpha)
  
  delta_testing = matrix(NA, nrow = n_testing, ncol = K)
  
  for(i in 1:n_testing){
    for(k in 1:K){
      delta_testing[i, k] = qK[k] * exp(X_testing[i,] %*% invSigma_alpha %*% muHat[k,] - (1/2)*muHat[k,] %*% invSigma_alpha %*% muHat[k,])
    }
  }
  
  # Find class that maximizes discriminant
  yHat_testing = classes[apply(delta_testing, 1, which.max)]
  
  # Compute testing accuracy
  accuracy[j] = mean(yHat_testing == Y_testing)
}
  
plot(alpha_values, accuracy, bty = 'n', 
     lwd = 2, xlab = 'alpha', ylab = 'accuracy')
  
which.max(accuracy)
alpha_values[43]  
max(accuracy)



### Masking data for sphering  
  
mydata = read.csv('masking-example.csv')
head(mydata)

n = dim(mydata)[1]

Y = as.matrix(mydata$Y, nrow = n)
X = model.matrix(Y ~ 0 + ., data=mydata)
p = dim(X)[2]

x1min = min(X[,1])
x1max = max(X[,1])

x2min = min(X[,2])
x2max = max(X[,2])

plot(X[Y == 1, 1], X[Y == 1, 2], col = 'forestgreen', pch = 2, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(X[Y == 2, 1], X[Y == 2, 2], col = 'darkorange1', pch = 2, lwd=2)
points(X[Y == 3, 1], X[Y == 3, 2], col = 'red', pch = 2, lwd=2)

legend(-0.2, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)

# Estimate SigmaHat

classes = levels(factor(Y))
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

# Estimate covariance matrix
SigmaHat = matrix(0, nrow = p, ncol = p)
for(k in 1:K){
  muMatrix = matrix(rep(muHat[k,], nK[k]), 
                    nrow = nK[k], ncol = p,
                    byrow = TRUE)
  SigmaHat = SigmaHat + t(X[Y == classes[k],] - muMatrix) %*% (X[Y == classes[k],] - muMatrix)
}
SigmaHat = SigmaHat / (n - K)

SigmaHat

### Apply Sphering

EVD = eigen(SigmaHat)
EVD

sqrtD = diag(1/sqrt(c(EVD$values)))
sqrtD

U = matrix(EVD$vectors, ncol = p)
U

Z = X %*% U %*% sqrtD

# Replot data and re-estimate SigmaHat

x1min = min(Z[,1])
x1max = max(Z[,1])

x2min = min(Z[,2])
x2max = max(Z[,2])

plot(Z[Y == 1, 1], Z[Y == 1, 2], col = 'forestgreen', pch = '.', cex = 3, lwd=2, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'X1', ylab='X2')
points(Z[Y == 2, 1], Z[Y == 2, 2], col = 'darkorange1', pch = '.', cex = 3, lwd=2)
points(Z[Y == 3, 1], Z[Y == 3, 2], col = 'red', pch = '.', cex = 3, lwd=2)

legend(-6, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('forestgreen', 'darkorange1', 'red'), pch = 2, bty='n', lwd=2, lty=NA)

# Estimate SigmaHat

classes = levels(factor(Y))
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
  muHat[k,] = apply(Z[Y == classes[k],], 2, mean)
}

# Estimate covariance matrix
SigmaHat = matrix(0, nrow = p, ncol = p)
for(k in 1:K){
  muMatrix = matrix(rep(muHat[k,], nK[k]), 
                    nrow = nK[k], ncol = p,
                    byrow = TRUE)
  SigmaHat = SigmaHat + t(Z[Y == classes[k],] - muMatrix) %*% (Z[Y == classes[k],] - muMatrix)
}
SigmaHat = SigmaHat / (n - K)

SigmaHat


muHat
points(muHat[1,1], muHat[1,2], col = 'forestgreen', 
       pch = 15, cex = 3)

points(muHat[2,1], muHat[2,2], col = 'darkorange1', 
       pch = 15, cex = 3)

points(muHat[3,1], muHat[3,2], col = 'red', 
       pch = 15, cex = 3)



###
# Perform PCA decomposition of muHat 
###

overall_mean = apply(muHat, 2, mean)
SigmaM_Hat = 1 / (K - 1) * (t(muHat) - overall_mean) %*% t(t(muHat) - overall_mean)

# Get Eigenvalue decomposition
EVD = eigen(SigmaM_Hat) 
D = diag(EVD$values)
V = matrix(EVD$vectors, nrow = p)


plot(Z[Y == 1, 1], Z[Y == 1, 2], col = 'forestgreen', pch = '.', lwd=1, ylim = c(x2min, x2max), xlim=c(x1min, x1max), bty='n', xlab = 'Z1', ylab='Z2', cex=2.5)
points(Z[Y == 2, 1], Z[Y == 2, 2], col = 'darkorange1', pch = '.', lwd=1, cex=2.5)
points(Z[Y == 3, 1], Z[Y == 3, 2], col = 'red', pch = '.', lwd=1, cex = 2.5)

points(muHat[1, 1], muHat[1, 2], col = 'forestgreen', pch = '.', cex=20)
points(muHat[2, 1], muHat[2, 2], col = 'darkorange1', pch = '.', cex=20)
points(muHat[3, 1], muHat[3, 2], col = 'red', pch = '.', cex=20)


# Convert PCA vectors into lines

xcoordinate = c(x1min, x1max)
ycoordinate = c(V[2, 1] / V[1, 1] * x1min, V[2, 1] / V[1, 1] * x1max) 
lines(xcoordinate, ycoordinate, lwd = 2)





###
# Vowel Example - Dimension Reduction
###


training_data = read.csv('vowel_training.csv')
head(training_data)

trainingX = model.matrix(y ~ 0 + ., data=training_data)
trainingY = factor(training_data$y)

classes = levels(trainingY)
n_training = length(trainingY)

testing_data = read.csv('vowel_testing.csv')
head(testing_data)

testingX = model.matrix(y ~ 0 + ., data=testing_data)
testingY = factor(testing_data$y)
n_testing = length(testingY)

K = length(classes)
p = dim(trainingX)[2]

nK = matrix(0, nrow = K)
qK = matrix(0, nrow = K)

# Get number of observations in each class
# and estimate proportions
for(k in 1:K){
  nK[k] = sum(trainingY == classes[k])
  qK[k] = nK[k] / n
}

# Estimate muHat
muHat = matrix(0, nrow = K, ncol = p)
for(k in c(1:K)){
  muHat[k,] = apply(trainingX[trainingY == classes[k],], 2, mean)
} 

# Pooled estimate covariance matrix
SigmaHat = matrix(0, nrow = p, ncol = p)
for(k in 1:K){
  muMatrix = matrix(rep(muHat[k,], nK[k]), 
                    nrow = nK[k], ncol = p,
                    byrow = TRUE)
  SigmaHat = SigmaHat + t(trainingX[trainingY == classes[k],] - muMatrix) %*% (trainingX[trainingY == classes[k],] - muMatrix)
}
SigmaHat = SigmaHat / (n - K)

# Get Eigenvalue decomposition
EVD = eigen(SigmaHat) 
D = diag(EVD$values)
U = matrix(EVD$vectors, nrow = p)

# Data in sphered coordinates
trainingZ = trainingX %*% U %*% diag(EVD$values^(-1/2)) 
testingZ = testingX %*% U %*% diag(EVD$values^(-1/2))

muHat = muHat %*% U %*% diag(EVD$values^(-1/2))

overall_mean = apply(muHat, 2, mean)
SigmaM_Hat = 1 / (K - 1) * (t(muHat) - overall_mean) %*% t(t(muHat) - overall_mean)

# Get Eigenvalue decomposition
EVD = eigen(SigmaM_Hat) 
V = matrix(EVD$vectors, nrow = p)


###
# Reduce to two dimensions
###

trainingW = trainingZ %*% V[, c(1:2)]

colours = rainbow(K)

plot(trainingW[trainingY == classes[1], 1], trainingW[trainingY == classes[1], 2], 
     xlab = 'First Component', ylab = 'Second Component', main='LDA dimension reduction',
     bty = 'n', col = colours[1], lwd=2, xlim = c(min(trainingW[,1]), max(trainingW[,1])),
     ylim=c(min(trainingW[,2]), max(trainingW[,2])))

for(k in c(2:K)){
  points(trainingW[trainingY == classes[k], 1], trainingW[trainingY == classes[k], 2], 
         col = colours[k], lwd=2)
}


### 
# Find optimal dimension
###

dimensions = c(2:p)
n_dimensions = length(dimensions)
accuracy = matrix(0, nrow = n_dimensions)

for(j in c(1:n_dimensions)){
  d = dimensions[j]
  
  trainingW = trainingZ %*% V[,c(1:d)]
  testingW = testingZ %*% V[,c(1:d)]
  
  # Estimate muHat
  muHat = matrix(0, nrow = K, ncol = d)
  for(k in c(1:K)){
    muHat[k,] = apply(trainingW[trainingY == classes[k],], 2, mean)
  } 
  
  # Estimate class probabilities
  q = matrix(0, nrow = k)
  for(k in c(1:K)){
    q[k] = mean(trainingY == classes[k])
  }
  
  # Compute discriminant values for testing data
  # Calculate delta values
  delta = matrix(0, nrow = n_testing, ncol = K)
  for(i in c(1:n_testing)){ 
    for(k in c(1:K)){
      delta[i,k] = q[k] * exp( -(1/2)*(t(testingW[i,]) - muHat[k,]) %*% t(t(testingW[i,]) - muHat[k,])) 
    }
  }
  
  yHat = apply(delta, 1, which.max)
  yHat = classes[yHat]
  accuracy[j] = mean(yHat == testingY)
}

plot(dimensions, accuracy, xlab = 'dimension', type = 'b', lwd = 2, cex = 1.25, bty = 'n')





