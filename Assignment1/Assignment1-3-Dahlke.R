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
# minimizing lambda = 1
