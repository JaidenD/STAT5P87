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
rescale <- function(x) (x-mean(x))/norm_vec(x)

x1 = rescale(x1)
x2 = rescale(x2)
x3 = rescale(x3)
y = y-mean(y)

X = cbind(1,x1,x2,x3)
beta1=rep(0,dim(X)[2])

LAR <- function(X, y, beta1) {
  ehat =  y - X %*% beta1
  c = t(X) %*% ehat # Correlations
  C = max(abs(c)) # Maximum correlation
  
  J = which(abs(c) == C) # Indices of maximum correlation
  
  # Compute how much we should increase β_j for all j ∈ J
  X_J = X[,J]

  # Compute some intermediate values
  G_J = t(X_J) %*% X_J
  ones = rep(1,dim(J))
  A_J = -1/sqrt(t(ones) %*% solve(G_J) %*% ones)
  w_J = A_J %*% solve(G_J) %*% ones
  u_J = X_J %*% w_J
  A = t(X) %*% u_J

  # Compute how much we should increase βj for all j ∈ J
  
}









