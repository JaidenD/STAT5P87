### STAT5P87 - Week 3 Script

mydata = read.csv('simple-classification-data.csv')

set.seed(0)

p = 2
n = 200
n_training = 100
n_testing = 100

training_index = sample(c(1:n), size = n_training, replace = FALSE)

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


### Apply ridge regression

lambda_values = seq(from = 0, to = 500, by = 1)
n_lambda_values = length(lambda_values)
accuracy = matrix(NA, nrow = n_lambda_values)

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
  accuracy[i] = mean(yHat == testingy)
}

plot(lambda_values, accuracy)

# Find optimal lambda
which.max(accuracy)
max(accuracy)
lambda = lambda_values[155]



### Plot decision boundary

# Fit a model using all data and optimal lambda
X = model.matrix(y ~ 0 + x1 + x2, data=mydata)
y = mydata$y

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


###
# Nonbinary Classification
###


mydata = read.csv('nonbinary-data.csv')
head(mydata)

n = dim(mydata)[1]
Y = as.matrix(mydata$Y, nrow = n)
X = model.matrix(Y ~ ., data=mydata)

plot(X[Y == 1, 2], X[Y == 1, 3], xlab = 'X1', ylab = 'X2', xlim = c(min(X[,2]), max(X[,2])), ylim = c(min(X[,3]), max(X[,3])), 
     bty = 'n', col = 'red', lwd = 2)

points(X[Y == 2, 2], X[Y == 2, 3], col = 'forestgreen', lwd = 2)
points(X[Y == 3, 2], X[Y == 3, 3], col = 'dodgerblue3', lwd = 2)

legend(-1.3, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('red', 'forestgreen', 'blue'), cex = 1.2, bty = 'n', lwd = 3, lty = NA, pch = 1)


# Decision boundary for Y = 1 vs. Y = 2
Y12 = Y[Y == 1 | Y == 2]
Y12[Y12 == 2] = 0

X12 = X[Y == 1 | Y == 2,]

B12 = solve(t(X12) %*% X12) %*% t(X12) %*% Y12

xcoordinate = c(min(X[,2]), max(X[,2]))
ycoordinate = c((0.5 - B12[1] - B12[2] * min(X[,2])) / B12[3], 
                (0.5 - B12[1] - B12[2] * max(X[,3])) / B12[3])
lines(xcoordinate, ycoordinate, lwd=2)


# Decision boundary for Y = 1 vs. Y = 3
Y13 = Y[Y == 1 | Y == 3]
Y13[Y13 == 3] = 0

X13 = X[Y == 1 | Y == 3,]
B13 = solve(t(X13) %*% X13) %*% t(X13) %*% Y13

xcoordinate = c(min(X[,2]), max(X[,2]))
ycoordinate = c((0.5 - B13[1] - B13[2] * min(X[,2])) / B13[3], 
                (0.5 - B13[1] - B13[2] * max(X[,3])) / B13[3])
lines(xcoordinate, ycoordinate, lwd=2)



# Decision boundary for Y = 2 vs. Y = 3
Y23 = Y[Y == 2 | Y == 3]
Y23[Y23 == 3] = 0
Y23[Y23 == 2] = 1

X23 = X[Y == 2 | Y == 3,]
B23 = solve(t(X23) %*% X23) %*% t(X23) %*% Y23

xcoordinate = c(min(X[,2]), max(X[,2]))
ycoordinate = c((0.5 - B23[1] - B23[2] * min(X[,2])) / B23[3], 
                (0.5 - B23[1] - B23[2] * max(X[,3])) / B23[3])
lines(xcoordinate, ycoordinate, lwd=2)


# Find the point of intersection between the two (three) lines

# (0.5 - B13[1] - B13[2] * X1) / B13[3] = (0.5 - B12[1] - B12[2] * X1) / B12[3]
# B12[3] * (0.5 - B13[1] - B13[2] * x1) = B13[3] * (0.5 - B12[1] - B12[2] * x1)
# B12[3] * 0.5 - B12[3] * B13[1] - B13[3] * 0.5 + B13[3] * B12[1] = B12[3] * B13[2] * x1 - B13[3] * B12[2] * x1
# x1 = (0.5 * (B12[3] - B13[3]) + B13[3] * B12[1] - B12[3] * B13[1]) / (B12[3] * B13[2] - B13[3] * B12[2])

x1_intercept = (0.5 * (B12[3] - B13[3]) + B13[3] * B12[1] - B12[3] * B13[1]) / (B12[3] * B13[2] - B13[3] * B12[2])
x2_intercept = (0.5 - B13[1] - B13[2] * x1_intercept) / B13[3]


plot(X[Y == 1, 2], X[Y == 1, 3], xlab = 'X1', ylab = 'X2', xlim = c(min(X[,2]), max(X[,2])), ylim = c(min(X[,3]), max(X[,3])), 
     bty = 'n', col = 'red', lwd = 2)

points(X[Y == 2, 2], X[Y == 2, 3], col = 'forestgreen', lwd = 2)
points(X[Y == 3, 2], X[Y == 3, 3], col = 'dodgerblue3', lwd = 2)

legend(-1.3, 1.2, c('Y = 1', 'Y = 2', 'Y = 3'), col = c('red', 'forestgreen', 'blue'), cex = 1.2, bty = 'n', lwd = 3, lty = NA, pch = 1)



# Decision boundary for Y = 1 vs. Y = 2

xcoordinate = c(min(X[,2]), x1_intercept)
ycoordinate = c((0.5 - B12[1] - B12[2] * min(X[,2])) / B12[3], 
                (0.5 - B12[1] - B12[2] * x1_intercept) / B12[3])
lines(xcoordinate, ycoordinate, lwd=2)


# Decision boundary for Y = 1 vs. Y = 3
xcoordinate = c(min(X[,2]), x1_intercept)
ycoordinate = c((0.5 - B13[1] - B13[2] * min(X[,2])) / B13[3], 
                (0.5 - B13[1] - B13[2] * x1_intercept) / B13[3])
lines(xcoordinate, ycoordinate, lwd=2)



# Decision boundary for Y = 2 vs. Y = 3

xcoordinate = c(x1_intercept, max(X[,2]))
ycoordinate = c((0.5 - B23[1] - B23[2] * x1_intercept) / B23[3], 
                (0.5 - B23[1] - B23[2] * max(X[,3])) / B23[3])
lines(xcoordinate, ycoordinate, lwd=2)


### Linear Discriminant Classifier

# Define indicator variables
Y1 = ifelse(Y == 1, 1, 0)
Y2 = ifelse(Y == 2, 1, 0)
Y3 = ifelse(Y == 3, 1, 0)

# Solve linear regression problem for 
# each model
B1 = solve(t(X) %*% X) %*% t(X) %*% Y1
B2 = solve(t(X) %*% X) %*% t(X) %*% Y2
B3 = solve(t(X) %*% X) %*% t(X) %*% Y3

# We can also lump all the Y's together
# to fit all models at once
allYs = cbind(Y1, Y2, Y3)
allBs = solve(t(X) %*% X) %*% t(X) %*% allYs

# Predict yHat for each model
yHat = X %*% allBs

# For example, to classify observation 1
yHat[1,]
which.max(yHat[1,])

# Perform classification for all observations
yHat = apply(yHat, 1, which.max)

# Compute training accuracy
mean(yHat == Y)



### Multiclass logistic regression
# with iris data
data(iris)

n = dim(iris)[1]
n_training = n*(2/3)

training_index = sample(c(1:n), size = n_training, replace = FALSE)

training_data = iris[training_index,]
testing_data = iris[-training_index,]

trainingX = model.matrix(Species ~ ., data=training_data)
testingX = model.matrix(Species ~ ., data=testing_data)

trainingy = training_data$Species
testingy = testing_data$Species

# Fit logistic model
model = glmnet(trainingX, trainingy, family = 'multinomial', 
               lambda = 0)

# Compute predictions
# type = 'class' returns the category predictions
# type = 'response' returns the probability estimates
yHat = predict(model, newx = testingX, type = 'class')
yHat

# Compute testing accuracy
mean(yHat == testingy)
