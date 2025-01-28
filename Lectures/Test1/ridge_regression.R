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
