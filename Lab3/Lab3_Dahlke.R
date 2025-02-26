data = read.csv("piecewise-data.csv")
head(data)

x = data$X
y = data$Y
odr = order(x)
x = x[odr]
y = y[odr]

#Part a

lambda = 0.01
#knots = unique(x)
knots = x
# Generate basis expansion
M = 1
K = length(knots)
n = length(y)

basis <- function(knots,x){
  # Create basis expansion
  lx = length(x)
  H = matrix(0, nrow = lx, ncol = lx)
  H[,1] = 1
  H[,2] = x
  
  dKMinus1 <- (pmax(0, x - knots[n-1])^3 - pmax(0, x - knots[n])^3) / (knots[n] - knots[n-1])
  dk <- outer(x, knots[1:(K-2)], function(x_val, knot) {
    (pmax(0, x_val - knot)^3 - pmax(0, x_val - knots[n])^3) / (knots[n] - knot)
  })
  H[, 3:K] <- dk - dKMinus1
  return(H)
}

# Construct the penalty matrix Omega, note that Omega is symmetric
OmegaX <- function(knots){
  l = length(knots)
  Omega = matrix(NA, nrow = l, ncol = l)
  
  Omega[1:2,] = 0
  Omega[,1:2] = 0
  for (i in 3:K) {
    for (j in 3:K) {
      if (j == i) {
        Omega[i, j] <- 12 * (knots[K-1] - knots[i-2])^2 / (knots[K] - knots[i-2])
      } else {
        Omega[i, j] <- 6 * (knots[K-1] - knots[i-2]) *
          (((3 * knots[j-2] - knots[K-1]) * knots[i-2]) - (knots[i-2]^2) +
             2 * knots[K] * (knots[K-1] - knots[j-2]) - knots[j-2] * knots[K-1]) /
          ((knots[K] - knots[j-2]) * (knots[K] - knots[i-2]))
        Omega[j, i] <- Omega[i, j]
      }
    }
  }
  return(Omega)
}

H = basis(knots,x)
Omega = OmegaX(knots)





B = solve(t(H)%*%H + lambda*Omega)%*%t(H)%*%y
yhat = H%*%B

# Part b
plot(x, y, main="Smooth Spline Fit", pch=16)
lines(x, yhat, col="goldenrod2",   lwd=2, lty=1)

# Part c
DF <- H %*% solve(t(H)%*%H + lambda*Omega) %*% t(H)
sum(diag(DF)) # degrees of freedom


