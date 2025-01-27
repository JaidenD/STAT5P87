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
























