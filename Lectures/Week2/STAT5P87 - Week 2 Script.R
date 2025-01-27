### STAT5P87 - Week 2
getwd()
# load the dataset
mydata = read.csv("~/STAT5P87/Lectures/Test1/prostate-data.csv")

# Look at the header
head(mydata)

# Extract the variables for this problem
mydata = mydata[,c(1, 4, 6, 9)]
head(mydata)

# Create a training-testing split
n = dim(mydata)[1]

# Make 75/25 split
0.75 * n
n_training = 73
n_testing = n - n_training

# Randomly generate training indices
set.seed(0)
training_index = sample(c(1:n), size = n_training, replace=FALSE)

# Split into training and testing dataframe
training_data = mydata[training_index,]
testing_data = mydata[-training_index,]

# "Manually" estimate the regression model
trainingY = training_data$lpsa
trainingX = model.matrix(lpsa ~ ., data=training_data)

bHat = solve(t(trainingX) %*% trainingX) %*% t(trainingX) %*% trainingY

# Evaluate the model using testing data
testingY = testing_data$lpsa
testingX = model.matrix(lpsa ~ ., data=testing_data)

# Make predictions
yHat = testingX %*% bHat

# Compute average testing loss
mean((testingY - yHat)^2)


### Using R's functions

# Fit model using training data
model = lm(lpsa ~ ., data=training_data)

# Predict testing data
yHat = predict(model, newdata = testing_data)

# Compute average testing loss
mean((testingY - yHat)^2)

# Suppose I want to predict y when x = c(1, 2, 3)
predict(model, newdata = data.frame(lcavol = 1, lbph = 2, lcp = 3))

## Visualize the predictions
plot(testingY, yHat, bty = 'n', ylab = 'Predicted Y', 
     xlab = 'True Y', cex = 1.5, lwd = 2)


## Is 0.6958 a "good" value of error?
# check what the error is for predicting 
# based on yBar

yHat = mean(trainingY)
mean((testingY - yHat)^2)

# 0.6958 vs. 1.5482
# about 50% of error reduced

# a "testing" R2 value
1 - (0.6958 / 1.5482)


### Best subset selection for k = 2

model1 = lm(lpsa ~ lcavol + lbph, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model1)
mean((trainingY - yHat)^2)
# 0.54

model2 = lm(lpsa ~ lcavol + lcp, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model2)
mean((trainingY - yHat)^2)
# 0.56

model3 = lm(lpsa ~ lcp + lbph, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model3)
mean((trainingY - yHat)^2)
# 0.59

# The best model with k = 2 is 
# lcavol + lbph with 0.54

### Now for k = 1

model1 = lm(lpsa ~ lcavol, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model1)
mean((trainingY - yHat)^2)
# 0.58

model2 = lm(lpsa ~ lcp, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model2)
mean((trainingY - yHat)^2)
# 0.91z

model3 = lm(lpsa ~ lbph, data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model3)
mean((trainingY - yHat)^2)
# 1.26

# The best k = 1 model is lcavol with 0.58


# With k = 3
model1 = lm(lpsa ~ ., data = training_data)
# With no "newdata" defaults to training data
yHat = predict(model1)
mean((trainingY - yHat)^2)
# 0.53

# The best k = 3 model is all inputs

# Use testing data to select k

model1 = lm(lpsa ~ lcavol, data=training_data)
yHat = predict(model1, newdata = testing_data)
mean((yHat - testingY)^2)
# 0.724

model2 = lm(lpsa ~ lcavol + lbph, data=training_data)
yHat = predict(model2, newdata = testing_data)
mean((yHat - testingY)^2)
# 0.691

model3 = lm(lpsa ~ lcavol + lbph + lcp, data=training_data)
yHat = predict(model3, newdata = testing_data)
mean((yHat - testingY)^2)
# 0.695

# Best model is k = 2
# lcavol + lbph with 0.691 average error





##### Start of Thursday


### Ridge regression

# training_data
# testing_data

# Number of inputs
p = 3

trainingY = training_data$lpsa
testingY = testing_data$lpsa

trainingX = model.matrix(lpsa ~ 0 + ., data=training_data)
testingX = model.matrix(lpsa ~ 0 + ., data=testing_data)

# Scale the data
# 2 for "keep columns" (would be 1 to keep rows)
xBar = apply(trainingX, 2, mean)
s = apply(trainingX, 2, sd)

# Scale the training data - notice this doesn't work
trainingZ = (trainingX - xBar) / s
testingZ = (testingX - xBar) / s

apply(trainingZ, 2, mean)


# Scale the training data
trainingZ = t((t(trainingX) - xBar) / s)
testingZ = t((t(testingX) - xBar) / s)

apply(trainingZ, 2, mean)
apply(trainingZ, 2, sd)

### Fit a ridge regression model with lambda = 1

lambda = 1

b0Hat = mean(trainingY)
bHat = solve(t(trainingZ) %*% trainingZ + lambda * diag(p)) %*% t(trainingZ) %*% (trainingY - mean(trainingY))

b0Hat
bHat

# Make predictions on testing data
yHat = b0Hat + testingZ %*% bHat

# Estimate mean-squared error
mean((testingY - yHat)^2)



### Use testing data to find the optimal lambda

lambda_values = seq(from = 5, to = 15, by = 0.01)
n_lambda_values = length(lambda_values)
mse = matrix(NA, nrow = n_lambda_values)

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
which.min(mse)
lambda_values[535]


### The optimal lambda is 10.34, go back and refit

lambda = 10.34
b0Hat = mean(trainingY)
bHat = solve(t(trainingZ) %*% trainingZ + lambda * diag(p)) %*% t(trainingZ) %*% (trainingY - mean(trainingY))

# Make predictions on testing data
yHat = b0Hat + testingZ %*% bHat

# Estimate mean-squared error
mean((testingY - yHat)^2)

b0Hat
bHat


### LASSO

# load glmnet for LASSO fitting
require(glmnet)

lambda = 1
b0Hat = mean(trainingY)
# Intercept false because we're doing it
# manually. glmnet would do it good on its own
# if I let it. 
fit = glmnet(trainingZ, trainingY, alpha = 1, 
               lambda = lambda, intercept = FALSE)
  
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

# Least-squares: 2.62, 0.77, 0.18, 0.13
# Ridge: 2.62, 0.64, 0.15, 0.18
# LASSO: 2.62, 0.73, 0.13, 0.10


