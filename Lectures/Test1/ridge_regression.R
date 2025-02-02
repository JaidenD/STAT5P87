### ridge regression############################################################
data = read.csv("~/STAT5P87/Lectures/Test1/prostate-data.csv")
data = data[,c(1, 4, 6, 9)]

# Make training-testing sizes
data_size = dim(data)[1]
training_size = ceiling(data_size * 0.75)
testing_size = data_size - training_size

# Pick subsets
set.seed(0)
training_index = sample(c(1:data_size), size = training_size, replace = FALSE)
training = data[training_index,]
testing = data[-training_index,]

trainingY = training$lpsa

lcavol = data$lcavol
lbph = data$lbph
lcp = data$lcp

# Regularize
lcavol = (lcavol - mean(lcavol))/sd(lcavol)
lbph = (lbph - mean(lbph))/sd(lbph)
lcp = (lcp - mean(lcp))/sd(lcp)


lambda_values = 10^seq(from = -2, to = 2, by = 0.1)

mse = matrix(NA, nrow = length(lambda_values))

for(i in 1:n_lambda_values){
  lambda = lambda_values[i]
  
  b0Hat = mean(trainingY)
  bHat = solve(t(trainingZ) %*% trainingZ + lambda * diag(p)) %*% t(trainingZ) %*% (trainingY - mean(trainingY))
  
  # Make predictions on testing data
  yHat = b0Hat + testingZ %*% bHat
  
  # Estimate mean-squared error
  mse[i] = mean((testingY - yHat)^2)
}


plot(lambda_values, mse, bty = 'n', 
     lwd = 2, cex = 1.2)

# What is the index that minimizes the vector
lambda = lambda_values[which.min(mse)]

b0Hat = mean(trainingY)
bHat = solve(t(trainingZ) %*% trainingZ + lambda * diag(p)) %*% t(trainingZ) %*% (trainingY - mean(trainingY))

# Make predictions on testing data
yHat = b0Hat + testingZ %*% bHat

# Estimate mean-squared error
mean((testingY - yHat)^2)

b0Hat
bHat
####LASSO############################################################
# load glmnet for LASSO fitting
require(glmnet)

lambda = 1
b0Hat = mean(trainingY)
# Intercept false because we're doing it
# manually. glmnet would do it good on its own
# if I let it. 
fit = glmnet(trainingZ, trainingY, alpha = 1, 
             lambda = 1, intercept = FALSE)
fit  
# Make predictions on testing data
yHat = b0Hat + predict(fit, testingZ)

# Estimate mean-squared error
mean((testingY - yHat)^2)


### Find the optimal lambda 

lambda_values = seq(from = 0, to = 0.1, by = 0.0005)
n_lambda_values = length(lambda_values)
mse = matrix(NA, nrow = n_lambda_values)

for(i in 1:n_lambda_values){
  lambda = lambda_values[i]
  
  b0Hat = mean(trainingY)
  fit = glmnet(trainingZ, trainingY, alpha = 1, 
               lambda = lambda, intercept = FALSE)
  # Make predictions on testing data
  yHat = b0Hat + predict(fit, testingZ)
  
  # Estimate mean-squared error
  mse[i] = mean((testingY - yHat)^2)
}

plot(lambda_values, mse, bty = 'n', 
     lwd = 2, cex = 1.2)

# What is the index that minimizes the vector
which.min(mse)
lambda_values[107]

lambda = 0.053
b0Hat = mean(trainingY)
fit = glmnet(trainingZ, trainingY, alpha = 1, 
             lambda = lambda, intercept = FALSE)
fit$beta
### Binary classification ###############################################################
data = read.csv('~/STAT5P87/Lectures/Test1/simple-classification-data.csv')

set.seed(0)

p = 2 # Binary classification
data_size = dim(data)[1]

# 50-50 training testing
training_split = 0.5
testing_split = 1 - training_split


training_index = sample(c(1:n), size = n*training_split, replace = FALSE)

# Split dataframe into training and testing
training_data = mydata[training_index,]
testing_data = mydata[-training_index,]

# Create relevant matrices and vectors
trainingX = model.matrix(y ~ 0 + x1 + x2, data=training_data)
testingX = model.matrix(y ~ 0 + x1 + x2, data=testing_data)

trainingy = training_data$y
testingy = testing_data$y

# Scale data
xBar = apply(trainingX, 2, mean)
s = apply(trainingX, 2, sd)

trainingX = t((t(trainingX) - xBar) / s)
testingX = t((t(testingX) - xBar) / s)

# Double check that I didn't make a mistake
apply(trainingX, 2, mean)
apply(trainingX, 2, sd)

## ridge regression classification #######################################################

# Define lambda range
lambda_values = seq(from = 0, to = 500, by = 1)
n_lambda_values = length(lambda_values)
accuracy = matrix(NA, nrow = n_lambda_values)

# For each lambda estimate and compute MSE
for(i in 1:n_lambda_values){
  lambda = lambda_values[i]
  
  # Estimate Betas
  b0Hat = mean(trainingy)
  bHat = solve(t(trainingX) %*% trainingX + lambda * diag(p)) %*% t(trainingX) %*% trainingy
  
  # Predict testingy
  yHat = b0Hat + testingX %*% bHat
  
  # Convert to class
  yHat[yHat >= 0.5] = 1
  yHat[yHat < 0.5] = 0
  
  # Compute accuracy
  accuracy[i] = mean(yHat == testingy) # How often does yHat correctly classify testingy -- compute the mean
}
plot(lambda_values, accuracy)

# Find optimal lambda
lambda = lambda_values[which.max(accuracy)]
max(accuracy)

### plot decision boundary
X = model.matrix(y ~ 0 + x1 + x2, data=data)
y = data$y

# Scale data
xBar = apply(X, 2, mean)
s = apply(X, 2, sd)
X = t((t(X) - xBar) / s)

# Estimate Betas
b0Hat = mean(trainingy)
bHat = solve(t(trainingX) %*% trainingX + lambda * diag(p)) %*% t(trainingX) %*% trainingy

### Initialize plot
x1range = c(min(X[,1]), max(X[,1]))
x2range = c(min(X[,2]), max(X[,2]))

plot(X[mydata$y == 0,1], X[mydata$y == 0,2],
     col = 'dodgerblue3', bty = 'n', xlab = 'x1', 
     ylab = 'x2', lwd = 2, xlim = x1range, 
     ylim = x2range)

points(X[mydata$y == 1,1], X[mydata$y == 1,2],
       col = 'darkorange2', lwd = 2)


# what is the decision boundary?
# P(y = 1|x) = b0 + b1*x1 + b2*x2
# Predict y = 1 when P(y = 1|x) >= 0.5
# Predict y = 1 when b0 + b1*x1 + b2*x2 >= 0.5
# x2 >= (0.5 - b0)/b2 - b1*x1/b2
b0Hat
bHat

x1 = c(-4, 4)
x2 = (0.5 - b0Hat)/bHat[2] - bHat[1]/bHat[2]*x1

lines(x1, x2, lwd = 2)
####################################################################################
#### Logistic Regression

mydata = read.csv('SAheart-data.csv')
head(mydata)


# Randomly select into training/testing with 70% training
n = dim(mydata)[1]
n * 0.7
n_training = 323
n_testing = n - n_training
training_index = sample(c(1:n), size = n_training, replace = FALSE)

training_data = mydata[training_index,]
testing_data = mydata[-training_index,]

# Create X matrix and y vector for training and testing data
trainingX = model.matrix(chd ~ ., data=training_data)
testingX = model.matrix(chd ~ ., data=testing_data)

trainingy = training_data$chd
testingy = testing_data$chd

# Initialize beta for iterative solution
p = dim(trainingX)[2]
bHat = matrix(0, nrow = p)

# Algorithm parameters
max_iterations = 100
threshold = 0.001

for(iter in 1:max_iterations){
  # Compute gradiant
  F = t(trainingX) %*% (trainingy - exp(trainingX %*% bHat) / (1 + exp(trainingX %*% bHat)))
  
  # Pr for Probability y = 1
  Pr = exp(trainingX %*% bHat) / (1 + exp(trainingX %*% bHat))
  
  # Create W matrix
  W = matrix(0, nrow = n_training, ncol = n_training)
  # set the diagnoal elements
  diag(W) = Pr * (1 - Pr)
  
  # Hessian
  H = -t(trainingX) %*% W %*% trainingX
  
  # Update bHat
  new_bHat = bHat - solve(H) %*% F
  
  # Check for convergence
  if(sum(abs(new_bHat - bHat)) < threshold){
    bHat = new_bHat
    break
  }
  bHat = new_bHat
}

# Check bHat and the number of iterations
bHat
iter

# Compute P(y = 1 | x)
Pr = exp(trainingX %*% bHat) / (1 + exp(trainingX %*% bHat))

# Predict y = 1 if Pr > 0.5
yHat = matrix(0, nrow = n_training)
yHat[Pr >= 0.5] = 1

# Training accuracy
mean(yHat == trainingy)

# Testing accuracy
Pr = exp(testingX %*% bHat) / (1 + exp(testingX %*% bHat))
yHat = matrix(0, nrow = n_testing)
yHat[Pr >= 0.5] = 1
mean(yHat == testingy)


### Using glmnet

require(glmnet)

# family = 'binomail' for classification
# lambda = 0 for no penalty
model = glmnet(trainingX, trainingy, family = 'binomial', lambda = 0)

# Verify training accuracy matches "manual" version
yHat = predict(model, newx = trainingX, type = 'class')
mean(yHat == trainingy)

# Verify testing accuracy matches "manual" version
yHat = predict(model, newx = testingX, type = 'class')
mean(yHat == testingy)

# Verify beta estimates are the same 
bHat
model$beta
model$a0



### Using glmnet with regularization

lambda_values = seq(from = 0, to = 0.1, by = 0.0001)
n_lambda_values = length(lambda_values)
accuracy = matrix(NA, nrow = n_lambda_values)

for(i in 1:n_lambda_values){
  lambda = lambda_values[i]
  
  # family = 'binomial' for classification
  # alpha = 1 for LASSO penalty
  model = glmnet(trainingX, trainingy, family = 'binomial', 
                 lambda = lambda, alpha = 1)
  
  # Compute testing accuracy
  yHat = predict(model, newx = testingX, type = 'class')
  accuracy[i] = mean(yHat == testingy)
}

plot(lambda_values, accuracy, bty = 'n')

# accuracy maximized for lambda = 0.0426
# with accuracy of 0.741
which.max(accuracy)
lambda_values[427]
max(accuracy)

# compare to lambda = 0
accuracy[1]
























































