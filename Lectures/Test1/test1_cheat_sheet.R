### subset selection############################################################
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
2
trainingY = training$lpsa

# subset selection k=3
model1 = lm(lpsa ~ .,data=training)
yhat1 = predict(model1, data = testing_data)

mean((trainingY - yhat1)^2)
# 0.534

# k=2
model2 = lm(lpsa ~ lcavol + lbph,data=training)
yhat2 = predict(model2, data = testing_data)

mean((trainingY - yhat2)^2)
# 0.545

model3 = lm(lpsa ~ lcavol + lcp,data=training)
yhat3 = predict(model3, data=testing)

mean((trainingY - yhat3)^2)
# 0.569

model4 = lm(lpsa ~ lcp + lbph,data=training)
yhat4 = predict(model4, data = testing_data)

mean((trainingY - yhat4)^2)
# 0.892

# k=1
model4 = lm(lpsa ~ lcavol,data=training)
yhat4 = predict(model4, data = testing_data)

mean((trainingY - yhat4)^2)
# 0.580

model5 = lm(lpsa ~ lcp,data=training)
yhat5 = predict(model5, data = testing_data)

mean((trainingY - yhat5)^2)
# 0.910

model6 = lm(lpsa ~ lbph,data=training)
yhat6 = predict(model6, data = testing_data)

mean((trainingY - yhat6)^2)
# 1.261



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


#### ASSIGNMENT#### 


### QUESTION 1
data = read.csv("~/STAT5P87/Assignment1/a1-q1-data.csv")

# Rename variable 'x' to 'x1'
data$variable[data$variable == 'x'] <- 'x1'

# Remove all rows corresponding to observation 2
is_two = (data$observation == 2)
data = data[-c(which(is_two==TRUE)),]

# Add rows to the data frame for a new observation 4 (x1 = 3, y = 2)
new_data = data.frame(c(4,4), c('x1','y'), c(3,2))
names(new_data) = c('observation', 'variable', 'value')

data = rbind(data,new_data)

# Swap rows so variable order is y,x1,y,x1,...
data[c(5, 6), ] = data[c(6, 5), ]

# Add rows to the data frame for a new variable x2 
#(x2(observation = 1) = 3, x2(observation = 3) = 1, x2(observation = 4) = 5)
newrow1 = data.frame(observation = 1, variable = "x2", value = "3")
newrow2 = data.frame(observation = 3, variable = "x2", value = "1")
newrow3 = data.frame(observation = 4, variable = "x2", value = "5")

data = rbind(data[1:2,], newrow1, data[3:4,], newrow2, data[5:6,], newrow3)

`value-squared` = as.numeric(data$value)^2
data = cbind(data,`value-squared`)
# ANS:
#observation variable value value-squared
#1            1        y     2             4
#2            1       x1     2             4
#12           1       x2     3             9
#5            3        y     6            36
#6            3       x1     3             9
#13           3       x2     1             1
#11           4        y     2             4
#21           4       x1     3             9
#14           4       x2     5            25



### QUESTION 2
data = read.csv("~/STAT5P87/Assignment1/a1-q2-data.csv")

unique_id = 0

for(school in unique(data$school)){
  # Filter for current school
  school_rows = (data$school == school)
  
  # Unique students in current school
  unique_students = unique(data$student[school_rows])
  
  # Map old student IDs to new student IDs
  student_mapping = setNames(seq(unique_id+1, unique_id + length(unique_students)), unique_students)
  
  # Update the student column for current school
  data$student[school_rows] = student_mapping[as.character(data$student[school_rows])]
  
  # Update counter
  unique_id = max(student_mapping)
}


data



### QUESTION 3
# Odd numbered observations as training, even numbered as testing
train = iris[(seq(nrow(iris))%%2==1),]
test = iris[(seq(nrow(iris))%%2==0),]

# Ridge Regression  Input: x1 = Sepal.Width, x2 = Petal.Length, x3 = Petal.Width
#                   Output: y = Sepal.Length

inputs = 3

train_features <- train[, c("Sepal.Width", "Petal.Length", "Petal.Width")]
test_features <- test[, c("Sepal.Width", "Petal.Length", "Petal.Width")]

# Compute training statistics
xBar <- apply(train_features, 2, mean)
s <- apply(train_features, 2, sd)

# Scale training and test data using training stats
X_train <- scale(train_features, center = xBar, scale = s)
X_test <- scale(test_features, center = xBar, scale = s)  # Use training stats!
y_train = train$Sepal.Length
y_test = test$Sepal.Length

# Optimize lambda
lambda_values = 10^seq(-1, 0.5, length = 100)
lambda_amount = length(lambda_values)
mse = matrix(NA, nrow = lambda_amount)

for(i in 1:lambda_amount){
  lambda = lambda_values[i]
  
  b0_hat = mean(y_train)
  b_hat = solve(t(X_train) %*% X_train + lambda * diag(inputs)) %*% t(X_train) %*% (y_train - b0_hat)
  
  # Make predictions on testing data
  y_pred = b0_hat + X_test %*% b_hat
  
  # Estimate mean-squared error
  mse[i] = mean((y_test - y_pred)^2)
}
plot(lambda_values, mse, bty = 'n',
     lwd = 2, cex = 1.2)

lambda_values[which(mse == min(mse))]
# ANS: minimizing lambda = 1



### QUESTION 4
### STAT5P87 - Assignment 1, Question 4 ###

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
  X = cbind(matrix(1, nrow = n), X)
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
  # Output: asas
  # 	Y: n x 1 column vector of output values 
  #
  n = dim(X)[1]
  E = matrix(rnorm(n, mean = 0, sd = sigma), nrow = n)
  Y = X %*% B + E
  return(Y)
}


###################################################################
### Exploring the bias-variance tradeoff for linear regression  
###################################################################

# Set randomizer seed
set.seed(0)

# Input dimension
p = 15

# How many times to iterate the simulation
n_iterations = 1000

# True Model: 
#
# b0 = 0, beta_j = 1/j
# X_{i,j} are iid Unif(0, 1)
# Y = XB + E
# where E ~ N(0, sigma^2)

B = matrix(0, nrow = p + 1)
for(j in c(1:p)){
  B[j + 1] = 1/j
}


# x_new value to be predicted
x_new = matrix(1/2, ncol = p + 1)
x_new[1] = 1

# Expected y_new value used to compute bias
Ey_new = x_new %*% B

# Initialize matrix to store results
hatY = matrix(0, nrow = n_iterations, ncol = p)

n = 30 # Training size
sigma = 1 # SD

for (iter in 1:n_iterations) {
  # Generate training data
  X = simulate_X(n,p)
  y = simulate_Y(X,B,sigma)
  
  for (k in 1:p) {
    X_subset = X[, 1:(k+1)]
    model = lm(y ~ X_subset - 1) # Fit without intercept
    coef = model$coefficients
    x_new_subset = x_new[,1:(k+1)]
    hatY[iter,k] = sum(x_new_subset*coef) # Prediction
  }
}

variance = apply(hatY, 2, var)
squared_bias = (colMeans(hatY) - as.numeric(Ey_new))^2
mse = squared_bias + variance

plot(1:p, mse, 
     type = "l", col = "red", lwd = 2,
     ylim = c(0, max(c(squared_bias, variance, mse))),
     xlab = "Number of predictors (k)", ylab = "Value",
     main = "Bias-Variance Trade-off in Linear Regression")
lines(1:p, squared_bias, col = "blue", lwd = 2, lty = 2)
lines(1:p, variance, col = "darkgreen", lwd = 2, lty = 3)
legend("topright", 
       legend = c("MSE", "Bias²", "Variance"),
       col = c("red", "blue", "darkgreen"),
       lty = c(1, 2, 3), lwd = 2, cex = 0.8)

###################### lab
# Implement the least-angle regression algorithm
data = read.csv("~/STAT5P87/Lab1/prostate-data.csv")

# Lasso regression  Input:   lcavol, lweight, age
#                   Output:  lpsa

# Read in data

# Inputs
x1 = data$lcavol
x2 = data$lweight
x3 = data$age

# Output
y = data$lpsa

# Compute norm
norm_vec <- function(x) sqrt(sum(x^2))

# Rescale
rescale <- function(x) (x-mean(x))/norm_vec(x-mean(x))

x1 = rescale(x1)
x2 = rescale(x2)
x3 = rescale(x3)
y = y-mean(y)

X = cbind(1,x1,x2,x3)

LAR_step <- function(X, y, beta_hat, tol = 1e-9) {
  ehat =  y - X %*% beta_hat
  c = t(X) %*% ehat # Correlations
  C = max(abs(c)) # Maximum correlation
  
  J = which(abs(abs(c)-C)<tol) # Indices of maximum correlation
  # Compute how much we should increase β_j for all j ∈ J
  sign_of_c = matrix(sign(c[J]), nrow = nrow(t(t(X[,J]))), ncol = ncol(t(t(X[,J]))), byrow = TRUE)
  X_J = sign_of_c*X[,J]
  
  # Compute some intermediate values
  G_J = t(X_J) %*% X_J
  ones = rep(1,length(J))
  A_J = as.numeric(1/sqrt(t(ones) %*% solve(G_J) %*% ones)) # Denominator is an inner product, so it is always scalar
  w_J = A_J * solve(G_J) %*% ones
  u_J = X_J %*% w_J
  a = t(X) %*% u_J
  
  gamma_candidates <- numeric(length(J))
  
  for (j in c(1,2,3,4)) {
    # Compute the two terms for gamma_hat
    term1 = ifelse(A_J - a[j] != 0, (C - c[j]) / (A_J - a[j]), Inf)
    term2 = ifelse(A_J + a[j] != 0, (C + c[j]) / (A_J + a[j]), Inf)
    
    term1 = if (term1 > 0) term1 else NA
    term2 = if (term2 > 0) term2 else NA
    
    gamma_candidates[j] = min(term1, term2, na.rm = TRUE)
  }
  
  # Find the minimum strictly positive value
  gamma_hat = min(gamma_candidates[gamma_candidates > 0], na.rm = TRUE)
  
  beta_hat[J] = beta_hat[J] + sign(c[J])*gamma_hat*w_J
  return(beta_hat)
}

beta_hat=rep(0,dim(X)[2]) # Initial value

steps = 3 # Iterations of the algorithm

beta_history = matrix(NA, nrow = dim(X)[2], ncol = steps+1)  # To save beta_hat at each step
beta_history[,1] = beta_hat # Add initial value

t = rep(0,steps+1)
t[0] = sum(abs(beta_hat))

for (step in 1:steps) {
  beta_hat = LAR_step(X, y, beta_hat)  # Perform one LAR step
  beta_history[,step+1] = beta_hat     # Save the current beta_hat
  t[step+1] = sum(abs(beta_history[,step+1]))
}

beta_history
t

y_range = range(beta_history[2:4, ])
plot(t, beta_history[2,], type = "b", col = "red", xlab = "t", ylab = "coefficient",ylim = y_range, bty="n")
lines(t, beta_history[3,], type = "b", col = "blue")
lines(t, beta_history[4,], type = "b", col = "green")
legend("topleft", legend = c("lcavol", "lweight", "age"), col = c("red", "blue", "green"), lty = 1, pch = 1,bty = "n")

