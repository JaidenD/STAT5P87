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

inputs = 3

lcavol = (training[,1]-mean(training[,1]))/(sd(training[,1]))
lbph = (training[,2]-mean(training[,2]))/(sd(training[,2]))
lcp = (training[,3]-mean(training[,3]))/(sd(training[,3]))

trainingZ = cbind(lcavol,lbph,lcp)
trainingY = training$lpsa

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
lambda_values[which.min(mse)]
# lambda = 10.34
