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
rescale <- function(x) (x-mean(x))/norm_vec(x-mean(x))

x1 = rescale(x1)
x2 = rescale(x2)
x3 = rescale(x3)
y = y-mean(y)

X = cbind(1,x1,x2,x3)

#----------------- Step 1 -----------------
beta_hat=rep(0,dim(X)[2])

ehat =  y - X %*% beta_hat
c = t(X) %*% ehat # Correlations
C = max(abs(c)) # Maximum correlation

J = which(abs(abs(c)-C)<1e-9) # Indices of maximum correlation

# Compute how much we should increase β_j for all j ∈ J
sign_of_c = matrix(sign(c[J]), nrow = nrow(t(t(X[,J]))), ncol = ncol(t(t(X[,J]))), byrow = TRUE)
X_J = sign_of_c*X[,J]

# Compute some intermediate values
G_J = t(X_J) %*% X_J
ones = rep(1,length(J))
A_J =  as.numeric(1/sqrt(t(ones) %*% solve(G_J) %*% ones))  # Denominator is an inner product, so it is always scalar
w_J = A_J * solve(G_J) %*% ones
u_J = X_J %*% w_J
a = t(X) %*% u_J

gamma_candidates <- numeric(length(J))

for (j in c(1,2,3,4)) {
  # Compute the two terms for gamma_hat
  term1 = ifelse(A_J - a[j] != 0, (C - c[j]) / (A_J - a[j]), Inf)
  term2 = ifelse(A_J + a[j] != 0, (C + c[j]) / (A_J + a[j]), Inf)
  
  term1 = if (term1 > 0) term1 else NA
  term2 = if (term2 > 0) term2 else NA
  
  gamma_candidates[j] = min(term1, term2, na.rm = TRUE)
}

# Find the minimum strictly positive value
gamma_hat = min(gamma_candidates[gamma_candidates > 0], na.rm = TRUE)

beta_hat[J] = beta_hat[J] + sign(c[J])*gamma_hat*w_J
beta_hat

#----------------- Step 2 -----------------
ehat =  y - X %*% beta_hat
c = t(X) %*% ehat # Correlations
C = max(abs(c)) # Maximum correlation

J = which(abs(abs(c)-C)<1e-9) # Indices of maximum correlation
# Compute how much we should increase β_j for all j ∈ J
sign_of_c = matrix(sign(c[J]), nrow = nrow(t(t(X[,J]))), ncol = ncol(t(t(X[,J]))), byrow = TRUE)
X_J = sign_of_c*X[,J]

# Compute some intermediate values
G_J = t(X_J) %*% X_J
ones = rep(1,length(J))
A_J = as.numeric(1/sqrt(t(ones) %*% solve(G_J) %*% ones)) # Denominator is an inner product, so it is always scalar
w_J = A_J * solve(G_J) %*% ones
u_J = X_J %*% w_J
a = t(X) %*% u_J

gamma_candidates <- numeric(length(J))

for (j in c(1,2,3,4)) {
  # Compute the two terms for gamma_hat
  term1 = ifelse(A_J - a[j] != 0, (C - c[j]) / (A_J - a[j]), Inf)
  term2 = ifelse(A_J + a[j] != 0, (C + c[j]) / (A_J + a[j]), Inf)
  
  term1 = if (term1 > 0) term1 else NA
  term2 = if (term2 > 0) term2 else NA
  
  gamma_candidates[j] = min(term1, term2, na.rm = TRUE)
}

# Find the minimum strictly positive value
gamma_hat = min(gamma_candidates[gamma_candidates > 0], na.rm = TRUE)

beta_hat[J] = beta_hat[J] + sign(c[J])*gamma_hat*w_J
beta_hat

#----------------- Step 3 -----------------
ehat =  y - X %*% beta_hat
c = t(X) %*% ehat # Correlations
C = max(abs(c)) # Maximum correlation

J = which(abs(abs(c)-C)<1e-9) # Indices of maximum correlation
# Compute how much we should increase β_j for all j ∈ J
sign_of_c = matrix(sign(c[J]), nrow = nrow(t(t(X[,J]))), ncol = ncol(t(t(X[,J]))), byrow = TRUE)
X_J = sign_of_c*X[,J]

# Compute some intermediate values
G_J = t(X_J) %*% X_J
ones = rep(1,length(J))
A_J = as.numeric(1/sqrt(t(ones) %*% solve(G_J) %*% ones)) # Denominator is an inner product, so it is always scalar
w_J = A_J * solve(G_J) %*% ones
u_J = X_J %*% w_J
a = t(X) %*% u_J

gamma_candidates <- numeric(length(J))

for (j in c(1,2,3,4)) {
  # Compute the two terms for gamma_hat
  term1 = ifelse(A_J - a[j] != 0, (C - c[j]) / (A_J - a[j]), Inf)
  term2 = ifelse(A_J + a[j] != 0, (C + c[j]) / (A_J + a[j]), Inf)
  
  term1 = if (term1 > 0) term1 else NA
  term2 = if (term2 > 0) term2 else NA
  
  gamma_candidates[j] = min(term1, term2, na.rm = TRUE)
}

# Find the minimum strictly positive value
gamma_hat = min(gamma_candidates[gamma_candidates > 0], na.rm = TRUE)

beta_hat[J] = beta_hat[J] + sign(c[J])*gamma_hat*w_J
beta_hat








