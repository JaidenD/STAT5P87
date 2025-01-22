# Odd numbered observations as training, even numbered as testing
train = iris[(seq(nrow(iris))%%2==1),]
test = iris[(seq(nrow(iris))%%2==0),]

# Ridge Regression  Input: x1 = Sepal.Width, x2 = Petal.Length, x3 = Petal.Width
#                   Output: y = Sepal.Length
# Compute norm
norm_vec <- function(x) sqrt(sum(x^2))

# Rescale
rescale <- function(x) (x-mean(x))/norm_vec(x-mean(x))

inputs = 3

# Rescale inputs
X_train = apply(train[, c("Sepal.Width", "Petal.Length", "Petal.Width")], 2, rescale)
X_test = apply(test[, c("Sepal.Width", "Petal.Length", "Petal.Width")], 2, rescale)

y_train = train$Sepal.Length
y_test = test$Sepal.Length

# Initial guess
lambda = 1

# Compute model coefficents
b0_hat = mean(y_train)
b_hat = solve(t(X_train) %*% X_train + lambda * inputs) %*% t(X_train) %*% (y_train - b0_hat)

# Make predictions
y_pred = b0_hat + X_test%*%b_hat

# Estimate mean-squared error
mean((y_test - y_pred)^2)

# Optimize lambda
lambda_values = 10^seq(-2, 2, length = 100)
lambda_amount = length(lambda_values)
mse = matrix(NA, nrow = lambda_amount)

for(i in 1:lambda_amount){
  lambda = lambda_values[i]
  
  b0_hat = mean(y_train)
  b_hat = solve(t(X_train) %*% X_train + lambda * inputs) %*% t(X_train) %*% (y_train - b0_hat)
  
  # Make predictions on testing data
  y_pred = b0_hat + X_test %*% b_hat
  
  # Estimate mean-squared error
  mse[i] = mean((y_test - y_pred)^2)
}

mse
plot(lambda_values, mse, bty = 'n', 
     lwd = 2, cex = 1.2)
