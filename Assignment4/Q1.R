# (j,h) = argmin_(j,h) Q(N_{j+1})+Q(N{j+2}), where Q is the quadratic loss.
find_best_split = function(X,y){
  # X - n x p input matrix
  # y - output
  
  p = dim(X)[1]
  n = dim(X)[2]

  loss = matrix(Inf,nrow=p-1+100,ncol=n+100)
  threshold = matrix(Inf,nrow=p-1+100,ncol=n+100)
  
  for (i in 1:n) {
    ord = order(X[,i])
    x = X[,i]
    
    x = x[ord]
    y = y[ord]
    
    for (h in 2:p) {
      N1 = x[1:(h-1)] 
      y1 = y[1:(h-1)]
      
      N2 = x[h:p]
      y2 = y[h:p]
      
      Q1 = quadratic_loss(N1,y1)
      Q2 = quadratic_loss(N2,y2)
      print(h)
      loss[h,i] = Q1+Q2
      threshold[h,i] = (x[h]+x[h-1])/2
    }
  }
  min_loss = min(loss)
  min_idx = arrayInd(which.min(loss), dim(loss))
  #return(list(min_loss,threshold[min_idx],min_idx[1]))
  return(list(min_loss))
  
  # For all inputs create a mesh of values from min(input_i) to max(input_i). 
  # Each mesh consists of all midpoints of observations
  # Compute Q(N_{i+1})+Q(N{i+2}) for all inputs and for all points of the mesh
  # return the (j,h) which minimize the function (h is the threshold)
}

custom_split <- find_best_split(X, y)

quadratic_loss = function(x,y){
  return((x-y)^2)
}




# test
# Load required package for rpart
library(rpart)

# Read in the data
mydata <- read.csv('prostate-data.csv')

# Use lcavol and lweight as predictors and lpsa as the response
x1 = mydata$lcavol
x2 = mydata$lweight
X = cbind(x1,x2)
y = mydata$lpsa

# Fit a regression tree using rpart (method "anova" for quadratic loss)
rpart_model <- rpart(lpsa ~ lcavol + lweight, data = mydata, method = "anova")

# Display the tree structure
print(rpart_model)
printcp(rpart_model)

# --- Extract the first split information from rpart ---
# The rpart object stores the splits in the frame and splits components.
# The first non-root node (usually row 2 of the frame) holds the first split.
first_split <- rpart_model$frame[2, ]
split_var <- as.character(first_split$var)

# rpart's splits component contains the split threshold.
# Here we extract the threshold from the first split row.
split_threshold <- rpart_model$splits[1, "index"]

cat("rpart first split:\n")
cat("  Variable:", split_var, "\n")
cat("  Threshold:", split_threshold, "\n\n")

# --- Now test your custom function ---
# Assuming your function 'find_best_split' is defined,
# it should take an input matrix X and a response vector y.
custom_split <- find_best_split(X, y)
cat("Custom function split result:\n")
print(custom_split)

















p = 97
n=2
loss = matrix(,nrow=p-1,ncol=n)
loss[,3]

