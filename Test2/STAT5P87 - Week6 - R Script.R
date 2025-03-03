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
