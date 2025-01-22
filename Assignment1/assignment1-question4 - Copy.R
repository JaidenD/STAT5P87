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
  # Output: 
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

# Size of training data
n_training = 30

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

sigma = 1

# x_new value to be predicted
x_new = matrix(1/2, ncol = p + 1)
x_new[1] = 1

# Expected y_new value used to compute bias
Ey_new = x_new %*% B

# Initialize matrix to store results
hatY = matrix(0, nrow = n_iterations, ncol = p)

