# Odd numbered observations as training, even numbered as testing
train = iris[(seq(nrow(data))%%2==1),]
test = iris[(seq(nrow(data))%%2==0),]

# Ridge Regression  Input: x1 = Sepal.Width, x2 = Petal.Length, x3 = Petal.Width
#                   Output: y = Sepal.Length
# Compute norm
norm_vec <- function(x) sqrt(sum(x^2))

# Rescale
rescale <- function(x) (x-mean(x))/norm_vec(x-mean(x))

inputs = 3

# Rescale data
X_train = apply(train[, c("Sepal.Width", "Petal.Length", "Petal.Width")], 2, rescale)
y_train = rescale(train$Sepal.Length)

X_test = apply(test[, c("Sepal.Width", "Petal.Length", "Petal.Width")], 2, rescale)
y_test = rescale(test$Sepal.Length)

# Initial guess
lambda = 1

b0_hat = mean(y_train)
b_hat = solve(t(X_train) %*% X_train + lambda * diag(p)) %*% t(X_train) %*% (y_train - b0Hat)













