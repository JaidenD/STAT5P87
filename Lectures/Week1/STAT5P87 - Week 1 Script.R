### STAT5P87 - Week 1

# The # sign is used to indicate a comment

# Find the current working directory
getwd()

# Change the working directory

# Once working directory is set
# load data using read.csv()
mydata = read.csv('simple-classification-data.csv')


# mydata is a "data.frame"
# look at the header of the data (first 6 rows)
head(mydata)


# Several ways to access data in a data frame

# Grab a specific column by name
mydata$x1

# We can also treat the data frame like a matrix
# and index into it

# To get the third row, second column (0.9993)
mydata[3,2]

# Grab all of column 2
mydata[,2]

# Grab all of row 3
mydata[3,]

# Get the type of each column
str(mydata)


# Create a `vector' or ordered set
# c() for `concatenate'
c(1, 2)
c(1, 5)

# The c() function can go into indexing 
# our dataframe. For example, rows 2, 3, 4
# all columns
mydata[c(2, 3, 4),]

# Create matrices from the dataframe
# Not necessary, but good practice to always
# specify the columns and rows
y = matrix(mydata$y, ncol = 1, nrow = 200)

# Can make the matrix explicitly
X = matrix(c(mydata$x1, mydata$x2), ncol = 2, nrow = 200)
X

# Or use the model.matrix() function
X = model.matrix(y ~ x1 + x2, data=mydata)

# Can use a . to mean "everything except the output"
X = model.matrix(y ~ ., data=mydata)
X

# Can suppress the intercept
X = model.matrix(y ~ 0 + ., data=mydata)
X



### Explore the data

head(mydata)

# Visualize the data
# use plot() to create a scatterplot
plot(mydata$x1, mydata$x2)

# Settings to customize your plot
plot(mydata$x1, mydata$x2, 
     xlab = 'x1', ylab = 'x2', 
     main = 'Scatterplot of x1 vs. x2', 
     lwd = 2, cex = 1, cex.lab = 1.5, 
     cex.axis = 1.5, bty = 'n', 
     col = 'black')

# I want to give different colour for each
# y value

# Only plot the values corresponding
# to y = 0

# to select only the y = 0 values
mydata$x1[mydata$y == 0]

plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     xlab = 'x1', ylab = 'x2', 
     main = 'Scatterplot of x1 vs. x2', 
     lwd = 2, cex = 1, cex.lab = 1.5, 
     cex.axis = 1.5, bty = 'n', 
     col = 'dodgerblue3', 
     xlim = c(min(mydata$x1), max(mydata$x1)), 
     ylim = c(min(mydata$x2), max(mydata$x2)))

# plot() creates a new figure
# use points() or lines() to add to an existing figure
points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], 
     lwd = 2, cex = 1, col = 'darkorange2')


# Compute median x1 value for y = 0 and  y = 1 observations
median_blue = median(mydata$x1[mydata$y == 0])
median_orange = median(mydata$x1[mydata$y == 1])

# Add x1 median lines to plot
lines(c(median_blue, median_blue), c(-5, 5), 
      col = 'dodgerblue3', lwd = 2)

lines(c(median_orange, median_orange), c(-5, 5), 
      col = 'darkorange2', lwd = 2)

# Define a threshold as a classification rule
# the midpoint between the two medians
threshold = (median_blue + median_orange) / 2

# Add threshold to plot
lines(c(threshold, threshold), c(-5, 5), 
      col = 'black', lwd = 2)

# Apply novel `median-midpoint-classifier' to training data
n = dim(mydata)[1]
yHat = matrix(0, nrow = n)
yHat[mydata$x1 < threshold] = 1

# Compute accuracy
mean(yHat == mydata$y)
yHat == mydata$y


### Implement the k-Nearest-Neighbours
# classifier

euclidean_distance = function(X, Y){
  # Euclidean distance between two 
  # equal-length vectors, X and Y
  # Input: 
  #   X: vector
  #   Y: vector
  # Output:
  #   distance between X and Y
  return(sqrt(sum((X - Y)^2)))
}

# test function
euclidean_distance(c(0, 0), c(3, 4))


## Back to kNN - classify xStar

xStar = c(0.5, 1)
n = dim(mydata)[1]
k = 10
X = model.matrix(y ~ 0 + ., data=mydata)

# Initiatlize distance vector
distances = matrix(0, ncol = n)

# Use for() loop to fill in distances
for(i in 1:n){
  distances[i] = euclidean_distance(xStar, X[i,])
}

# Sort distances (smallest to largest by default)
sorted_distances = sort(distances)
sorted_distances

# Identify indices of k nearest neighbours
which(distances <= sorted_distances[k])

# Look at corresponding y values
mydata$y[which(distances <= sorted_distances[k])]

# Check which is the majority
# if average > 0.5, then predict y = 1, otherwise, y = 0
mean(mydata$y[which(distances <= sorted_distances[k])])

# Take the above, and put it into a function
kNN = function(xStar, y, X, k){
  # Inputs:
  #   xStar: input vector to be classified
  #   y: 0/1 training outputs
  #   X: training inputs in matrix form
  #   k: positive integer, number of neighbours
  # Outputs:
  #   yHat: 0/1 predicted output
  n = dim(X)[1]
  distances = matrix(0, ncol = n)
  for(i in 1:n){
    distances[i] = euclidean_distance(xStar, X[i,])
  }
  sorted_distances = sort(distances)
  if(mean(y[which(distances <= sorted_distances[k])]) > 0.5){
    yHat = 1
  }else{
    yHat = 0
  }
  return(yHat)
}

# test function
kNN(xStar, mydata$y, X, 10)


## Compute training accuracy

n = dim(mydata)[1]
X = model.matrix(y ~ 0 + ., data=mydata)
yHat = matrix(0, ncol = n)
k = 10

# Predict y for each training observations
for(i in 1:n){
  yHat[i] = kNN(X[i,], mydata$y, X, k)
}

# Compute accuracy
mean(yHat == mydata$y)

### Visualize the decision boundary
# added post-lecture

# grid in each dimension
x_grid = seq(from=-1.75, to=2.75, by=0.05)
y_grid = seq(from=-1.75, to=2.75, by=0.05)

n_grid_points = length(x_grid)

# initialize 2D grid output
grid = matrix(NA, nrow = n_grid_points, ncol = n_grid_points)

# For each grid data point, apply kNN
for(i in c(1:n_grid_points)){
  for(j in c(1:n_grid_points)){
    grid[i, j] = kNN(c(x_grid[i], y_grid[j]), mydata$y, X, k)
  }
}

# Identify grid indices where we predict y = 0 or y = 1
# the arguement arr.ind = TRUE keeps the results as a
# 2-dimensional index, otherwise, it is given in 1 dimension
blue_index = which(grid == 0, arr.ind=TRUE)
orange_index = which(grid == 1, arr.ind=TRUE)

# colour-coded plot
plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], xlab = 'x1', ylab = 'x2', 
     bty = 'n', col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))
points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], xlab = 'x1', ylab = 'x2', 
       bty = 'n', col = 'orange', lwd = 2)

# Add colour-coded grid points
points(x_grid[blue_index[,1]], y_grid[blue_index[,2]], cex=2, pch='.', col='blue')
points(x_grid[orange_index[,1]], y_grid[orange_index[,2]], cex=2, pch='.', col='orange')


### Try different values of k

n = dim(mydata)[1]
X = model.matrix(y ~ 0 + ., data=mydata)

# Initialize accuracy for each k to be tested
accuracy = matrix(0, ncol = 15)

# Compute accuracy in a for() loop
for(k in 1:15){
  yHat = matrix(0, ncol = n)
  for(i in 1:n){
    yHat[i] = kNN(X[i,], mydata$y, X, k)
  }
  accuracy[k] = mean(yHat == mydata$y)
}

# Plot accuracy
plot(c(1:15), accuracy, bty = 'n', 
     xlab = 'k', ylab = 'accuracy', 
     cex.lab = 1.5, lwd = 2, cex.axis = 1.5)



### Try different values of k
# with training/testing split

n = dim(mydata)[1]
X = model.matrix(y ~ 0 + ., data=mydata)

# set.seed() for reproducibility
set.seed(0)
training_index = sample(c(1:n), size = 150)

# Split X into training and testing
X_training = X[training_index,]
X_testing = X[-training_index,]

# Split y into training and testing
y_training = mydata$y[training_index]
y_testing = mydata$y[-training_index]

# training and testing sample sizes
n_training = dim(X_training)[1]
n_testing = dim(X_testing)[1]

# Initialize accuracy, and compute testing accuracy for each k
accuracy = matrix(0, ncol = 15)
for(k in 1:15){
  yHat = matrix(0, ncol = n_testing)
  for(i in 1:n_testing){
    yHat[i] = kNN(X_testing[i,], y_training, X_training, k)
  }
  accuracy[k] = mean(yHat == y_testing)
}

# Plot testing accuracy
plot(c(1:15), accuracy, bty = 'n', 
     xlab = 'k', ylab = 'accuracy', 
     cex.lab = 1.5, lwd = 2, cex.axis = 1.5)

# The value of k that maximizes testing accuracy
which.max(accuracy)
