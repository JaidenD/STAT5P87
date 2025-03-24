
####################
########## Question One
####################


#####
# Part A: Create a plot to visualize the two-dimensional region structure
#####

# Load data
df <- data = read.csv("simple-classification-data.csv")
head(df) # manually inspect the data

assign_region <- function(x1, x2) {
  # Identify the region of a point
  # returns such region
  if (x1 <= 0.5 && x2 <= 1) {
    return(1)  # R1
  } else if (x1 <= 0.5 && x2 > 1) {
    return(2)  # R2
  } else if (x1 > 0.5 && x2 <= 1) {
    return(3)  # R3
  } else {
    return(4)  # R4
  }
}

# Assign color to each data point by-region
df$region <- mapply(assign_region, df$x1, df$x2)
df$region <- factor(df$region)  # Colors 1, 2, 3, 4

# Plot the data
plot(
  df$x2 ~ df$x1,
  col = df$region,          # color by region
  pch = 19,                 # solid dot
  xlab = "x1",
  ylab = "x2",
  main = "Two-Dimensional Region Structure"
)
abline(v = 0.5, lty = 9, lwd = 2) # Draw boundary lines (vertical)
abline(h = 1,   lty = 9,lwd = 2) # (horizontal)
legend("topright", legend = c("Region One",
                              "Region Two", 
                              "Region Three",
                              "Region Four"), col = 1:4, pch = 19, title = "Region")





#####
# Part B: Fit a piecewise-constant model and get training accuracy
#####

# Since each region is independent of others as there is no continuity requirement
#    we can classify regions one at a time
# Since we are using a piecewise model that is constant for 2-class classification,
#    lets classify each data point to the class which is most likely to appear 
#    in each region (mode)

region_modes <- tapply(df$y, df$region, function(x) {
  # Returns the mode for each region specified
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
})

# classify based on mode
df$y_pred <- region_modes[df$region]

# Compute Accuracy
accuracy <- mean(df$y_pred == df$y)
accuracy
### Our accuracy is 0.69





#####
# Part C: Continuous Piecewise Linear Model 
#####

df$x1_trunc <- pmax(0, df$x1 - 0.5)  # (x1 - 0.5) else 0
df$x2_trunc <- pmax(0, df$x2 - 1)   # (x2 - 1) else 0

# Fit a linear model (lm) with piecewise terms
#     Model: y = b0 + b1*x1 + b2*x2 + b3*x1_trunc + b4*x2_trunc
model <- lm(y ~ x1 + x2 + x1_trunc + x2_trunc, data = df)
#summary(model)

# Visualize the model to confirm the piecewise linear continuity
x1_seq <- seq(min(df$x1), max(df$x1), length.out = 50)
x2_seq <- seq(min(df$x2), max(df$x2), length.out = 50)
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

# Compute truncated terms on this grid
grid$x1_trunc <- pmax(0, grid$x1 - 0.5)
grid$x2_trunc <- pmax(0, grid$x2 - 1)

# Predict y-hat over the grid
grid$pred <- predict(model, newdata = grid)
z_mat <- matrix(grid$pred, 
                nrow = length(x1_seq), 
                ncol = length(x2_seq))

# 3D Plot of the model
persp(
  x = x1_seq, 
  y = x2_seq,
  z = z_mat,
  xlab = "x1",
  ylab = "x2",
  zlab = "Predicted y",
  theta = 30,    # rotation angle
  phi = 20,      # elevation angle
  expand = 0.5,  
  ticktype = "detailed"
)

# Training Accuracy

pred_linear <- predict(model, newdata = df)

# Convert continuous prediction "probabilities" to binary via threshold = 0.5
y_pred <- ifelse(pred_linear > 0.5, 1, 0)

# Compute training accuracy
accuracy <- mean(y_pred == df$y)
accuracy

### Our accuracy is 0.86%





####################
########## Question Two
####################


# Defining Each Basis Function Matrix

Xmat_linear <- function(x) {
  # Returns a vector of Linear Basis Functions
  cbind(1, x)
}

Xmat_cubic <- function(x) {
  # Returns a vector of Cubic Basis Functions
  cbind(1, x, x^2, x^3)
}

Xmat_cspline <- function(x, knots = c(1/3, 2/3)) {
  # Returns a Truncated Cubic Basis
  
  # truncate terms
  tcub <- function(x, k) {
    # truncated Cubic Function
    p <- x - k
    p[p<0] <- 0
    return(p^3)
  }
  
  # Form the basis
  k1 <- knots[1]
  k2 <- knots[2]
  return(cbind(
    1,
    x,
    x^2,
    x^3,
    tcub(x, k1),
    tcub(x, k2))
  )
}

Xmat_nspline <- function(x, knots = c(0.1,0.26,0.42,0.58,0.74,0.9)) {
  # Returns a matrix of natural Spline Basis Functions (cubic)
  k <- knots
  k1 <- k[1]
  kK <- k[length(k)]
  # start with columns for 1, x:
  Xmat <- cbind(1, x)
  
  # For j = 1..(K-1) define N_j(x):
  for(j in seq_len(length(k)-1)) {
    kj <- k[j]
    num  <- tcub(x, kj) - tcub(x, kK) * ((kj - k1)/(kK - k1))
    den  <- (kK - kj)
    Xmat <- cbind(Xmat, num/den)
  }
  # For j = K define N_K(x):
  K_ <- length(k)
  numK <- tcub(x, kK)
  denK <- (kK - k[K_-1])  # difference to the second-last knot
  Xmat <- cbind(Xmat, numK/denK)
  return(Xmat)
}

### Perform the simulation 
set.seed(0)

n <- 50 # number of training points
B <- 200 # number of Monte Carlo replications

# Generate Training Data (x): Fix this because we want pointwise variance estimates
X_train <- seq(0, 1, length.out = n)

# Storing the pointwise variances
var_linear  <- numeric(n)
var_cubic   <- numeric(n)
var_cspline <- numeric(n)
var_nspline <- numeric(n)

# We'll store the fitted function for each model in an n x B matrix
fits_lin  <- matrix(0, nrow = n, ncol = B)
fits_cub  <- matrix(0, nrow = n, ncol = B)
fits_cspl <- matrix(0, nrow = n, ncol = B)
fits_nspl <- matrix(0, nrow = n, ncol = B)

# Loop over B simulations
for(b in 1:B) {
  # Simulate Y
  eps <- rnorm(n, mean=0, sd=1)
  Y_train <- 3*X_train^3 - 5*X_train^2 + X_train - 3 + eps
  
  # 1) Fit Linear
  Xm_lin <- Xmat_linear(X_train)
  coef_lin <- lm.fit(Xm_lin, Y_train)$coefficients
  fits_lin[, b] <- Xm_lin %*% coef_lin
  
  # 2) Fit Cubic polynomial
  Xm_cub <- Xmat_cubic(X_train)
  coef_cub <- lm.fit(Xm_cub, Y_train)$coefficients
  fits_cub[, b] <- Xm_cub %*% coef_cub
  
  # 3) Fit Cubic Spline (knots 1/3, 2/3)
  Xm_cspl <- Xmat_cspline(X_train, knots=c(1/3, 2/3))
  coef_cspl <- lm.fit(Xm_cspl, Y_train)$coefficients
  fits_cspl[, b] <- Xm_cspl %*% coef_cspl
  
  # 4) Fit Natural Cubic Spline (knots=0.1,0.26,0.42,0.58,0.74,0.9)
  Xm_nspl <- Xmat_nspline(X_train, knots=c(0.1, 0.26, 0.42, 0.58, 0.74, 0.9))
  coef_nspl <- lm.fit(Xm_nspl, Y_train)$coefficients
  fits_nspl[, b] <- Xm_nspl %*% coef_nspl
}

# Now compute pointwise variance across the B fits
var_lin  <- apply(fits_lin,  1, var)
var_cub  <- apply(fits_cub,  1, var)
var_cspl <- apply(fits_cspl, 1, var)
var_nspl <- apply(fits_nspl, 1, var)

# Make the plot from the slides
plot(NULL, 
     xlim = c(0,1), 
     ylim = c(0, max(var_lin, var_cub, var_cspl, var_nspl)*1.1),
     xlab = "X", 
     ylab = "Pointwise Variances",
     main = "Comparison of Pointwise Variances (B = 200 Replications)")

points(X_train, var_lin,  col="orange", pch=19, cex=.8)
lines(X_train, var_lin,   col="orange", lty=1, lwd=2)

points(X_train, var_cub,  col="red",    pch=19, cex=0.8)
lines(X_train, var_cub,   col="red",    lty=1, lwd=2)

points(X_train, var_cspl, col="green",  pch=19, cex=0.8)
lines(X_train, var_cspl,  col="green",  lty=1, lwd=2)

points(X_train, var_nspl, col="blue",   pch=19, cex=0.8)
lines(X_train, var_nspl,  col="blue",   lty=1, lwd=2)

legend("topleft",
       legend=c("Linear","Cubic","Cubic Spline","Natural Spline"),
       col=c("orange","red","green","blue"),
       lty=1, lwd=2)



####################
########## Question Three
####################



## For Cross Validation:

make_folds = function(Y, nFolds, stratified = FALSE, seed = 0){
  # K-Fold cross validation
  # Input:
  #   Y (either sample size, or vector of outputs)
  #   stratified (boolean): whether the folds should 
  #     be stratified. If TRUE then Y should be a vector of outputs
  # Output: list of vectors of fold indices
  if(stratified & length(Y) == 1){
    stop('For stratified folds, Y must be a vector of outputs')
  }
  n = ifelse(length(Y) > 1, length(Y), Y)
  index = c(1:n)
  if(stratified){
    Y = factor(Y)
    classes = levels(Y)
    nClasses = length(classes)
    if(nClasses == 1){
      stop('stratified requires more than one class')
    }
    classfolds = list()
    for(class in 1:nClasses){
      classfolds[[class]] = list()
      classIndex = index[Y == classes[class]]
      n_class = sum(Y == classes[class])
      n_per_fold = floor(n_class / nFolds)
      shuffled_index = sample(classIndex)
      for(fold in c(1:(nFolds - 1))){
        classfolds[[class]][[fold]] = shuffled_index[c((1 + (fold - 1) * n_per_fold):(fold * n_per_fold))]
      }
      classfolds[[class]][[nFolds]] = shuffled_index[c(((nFolds - 1)*n_per_fold + 1):n_class)]
    }
    folds = list()
    for(fold in 1:nFolds){
      folds[[fold]] = classfolds[[1]][[fold]]
      for(class in 2:nClasses){
        folds[[fold]] = c(folds[[fold]], classfolds[[class]][[fold]])
      }
    }
  }else{
    folds = list()
    n_per_fold = floor(n / nFolds)
    shuffled_index = sample(index)
    for(fold in c(1:(nFolds - 1))){
      folds[[fold]] = shuffled_index[c((1 + (fold - 1) * n_per_fold):(fold * n_per_fold))]
    }  
    folds[[nFolds]] = shuffled_index[c(((nFolds - 1)*n_per_fold + 1):n)]
  }
  return(folds)
}



