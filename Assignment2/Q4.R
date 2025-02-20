library(glmnet)
data = read.csv("a2-vowel-data.csv")

head(data)

X <- model.matrix(y ~ x1 +x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data = data) # Input
y <- data$y # Output

set.seed(0)

fit <- cv.glmnet(X, y, family = "multinomial", alpha = 1, nfolds = 5, type.measure = "class")

lambda <- fit$lambda
accuracy <- 1 - fit$cvm # accuracy = 1 - misclassifications

best_lambda <- fit$lambda.min # Best Lambda: 0.0004371341
max_accuracy <- max(accuracy) # Maximum accuracy: 0.6