### STAT5P87 - Week10.R ###


#### 
# Decision Tree Example
####

# Load the data
mydata = read.csv('simple-classification-data.csv')
n = dim(mydata)[1]

range_x1 = c(min(mydata$x1), max(mydata$x1))
range_x2 = c(min(mydata$x2), max(mydata$x2))

plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     main = 'Scatterplot', xlab = 'x1', ylab = 'x2', bty='n', 
     col = 'blue', lwd = 2, xlim = range_x1, ylim = range_x2)

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'orange', 
       lwd = 2, pch = 2)


# Create translucent colours for plotting
require(graphics)

col2rgb('orange')
my_orange = rgb(255, 165, 0, max = 255, alpha = 25)

col2rgb('blue')
my_blue = rgb(0, 0, 255, max = 255, alpha = 25)


###
# N0 Split based on a threshold of x1 > 0.5
###

# threshold for split
h0 = 0.5

# estimate "c1" and "c2"
mean(mydata$y[mydata$x1 < h0])
mean(mydata$y[mydata$x1 >= h0])
# when x1 < h0 we predict 1, otherwise we predict 0

# estimate accuracy
(sum(mydata$y[mydata$x1 < h0] == 1) + 
    sum(mydata$y[mydata$x1 >= h0] == 0)) / n


plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     main = 'Scatterplot', xlab = 'x1', ylab = 'x2', bty='n', 
     col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'orange', 
       lwd = 2, pch = 2)

rect(range_x1[1], range_x2[1], h0, range_x2[2], col = my_orange, lwd = 3)
rect(h0, range_x2[1], range_x1[2], range_x2[2], col = my_blue, lwd = 3)


###
# Split N1 based on a threshold of x2 > -0.2
###

h1 = -0.2

# check predictions for each node
mean(mydata$y[mydata$x1 < h0 & mydata$x2 < h1])
mean(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1])
mean(mydata$y[mydata$x1 >= h0])

# compute accuracy
(sum(mydata$y[mydata$x1 < h0 & mydata$x2 < h1] == 0) + 
    sum(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1] == 1) + 
    sum(mydata$y[mydata$x1 >= h0] == 0)) / n

# plot partition
plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     main = 'Scatterplot', xlab = 'x1', ylab = 'x2', bty='n', 
     col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'orange', 
       lwd = 2, pch = 2)

rect(range_x1[1], h1, h0, range_x2[2], col = my_orange, lwd = 3)
rect(range_x1[1], range_x2[1], h0, h1, col = my_blue, lwd = 3)
rect(h0, range_x2[1], range_x1[2], range_x2[2], col = my_blue, lwd = 3)


###
# Split N2 based on x2 > 0.1
###

h2 = 0.1

# check predictions for each node
mean(mydata$y[mydata$x1 < h0 & mydata$x2 < h1])
mean(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1])

mean(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2])
mean(mydata$y[mydata$x1 >= h0 & mydata$x2 < h2])

# compute accuracy
(sum(mydata$y[mydata$x1 < h0 & mydata$x2 < h1] == 0) + 
    sum(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1] == 1) + 
    sum(mydata$y[mydata$x1 >= h0 & mydata$x2 < h2] == 0) + 
    sum(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2] == 1)) / n


# plot partition
plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     main = 'Scatterplot', xlab = 'x1', ylab = 'x2', bty='n', 
     col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'orange', 
       lwd = 2, pch = 2)

rect(range_x1[1], h1, h0, range_x2[2], col = my_orange, lwd = 3)
rect(range_x1[1], range_x2[1], h0, h1, col = my_blue, lwd = 3)

rect(h0, range_x2[1], range_x1[2], h2, col = my_blue, lwd = 3)
rect(h0, h2, range_x1[2], range_x2[2], col = my_orange, lwd = 3)


###
# Split N6 based on x1 > 1.4
###

h6 = 1.4

# check predictions for each node
mean(mydata$y[mydata$x1 < h0 & mydata$x2 < h1])
mean(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1])

mean(mydata$y[mydata$x1 >= h0 & mydata$x2 < h2])

mean(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2 & mydata$x1 >= h6])
mean(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2 & mydata$x1 < h6])

# compute accuracy
(sum(mydata$y[mydata$x1 < h0 & mydata$x2 < h1] == 0) + 
    sum(mydata$y[mydata$x1 < h0 & mydata$x2 >= h1] == 1) + 
    sum(mydata$y[mydata$x1 >= h0 & mydata$x2 < h2] == 0) + 
    sum(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2 & mydata$x1 < h6] == 1) + 
    sum(mydata$y[mydata$x1 >= h0 & mydata$x2 >= h2 & mydata$x1 >= h6] == 0)) / n


# plot partition
plot(mydata$x1[mydata$y == 0], mydata$x2[mydata$y == 0], 
     main = 'Scatterplot', xlab = 'x1', ylab = 'x2', bty='n', 
     col = 'blue', lwd = 2, xlim = c(min(mydata$x1), max(mydata$x1)),
     ylim = c(min(mydata$x2), max(mydata$x2)))

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'orange', 
       lwd = 2, pch = 2)

rect(range_x1[1], h1, h0, range_x2[2], col = my_orange, lwd = 3)
rect(range_x1[1], range_x2[1], h0, h1, col = my_blue, lwd = 3)

rect(h0, range_x2[1], range_x1[2], h2, col = my_blue, lwd = 3)

rect(h0, h2, h6, range_x2[2], col = my_orange, lwd = 3)
rect(h6, h2, range_x1[2], range_x2[2], col = my_blue, lwd = 3)


#############
# Growing a Regression Tree
#############

require(rpart)

mydata = read.csv('prostate-data.csv')

# Grow decision tree
# use rpart.control to set the growth parameters
fit = rpart(lpsa ~ lcavol + lweight, data = mydata, 
            control = rpart.control(maxdepth = 2))
plot(fit)
text(fit, cex = 1.25)
fit

# Finding the sequence of subtrees 


# Pruning the "left" node (lcavol < -0.0486)
temp = mydata[mydata$lcavol < 2.462,]
n = dim(temp)[1]
y = temp$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[temp$lcavol < -0.4786] = 0.6017
yHat[temp$lcavol >= -0.4786] = 2.327

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 1



# Pruning the "right" node (lcavol < 2.794)
temp = mydata[mydata$lcavol >= 2.462,]
n = dim(temp)[1]
y = temp$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[temp$lcavol < 2.794] = 3.284
yHat[temp$lcavol >= 2.794] = 4.203

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 1


# Pruning the "root" node
n = dim(mydata)[1]
y = mydata$lpsa

Rnode = sum((y - mean(y))^2)

yHat = predict(fit) 
Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 3

# the weakest link is pruning the "right node"
# with "effective alpha" 4.427103


### Find the second weakest link

# Pruning the "left" node (lcavol < -0.0486)
temp = mydata[mydata$lcavol < 2.462,]
n = dim(temp)[1]
y = temp$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[temp$lcavol < -0.4786] = 0.6017
yHat[temp$lcavol >= -0.4786] = 2.327

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 1


# Pruning the "root" node
n = dim(mydata)[1]
y = mydata$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[mydata$lcavol < -0.4786] = 0.6017
yHat[mydata$lcavol >= -0.4786 & mydata$lcavol < 2.462] = 2.327
yHat[mydata$lcavol >= 2.462] = 3.7654

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch) / 2

# the second weakest link is the "left node" 
# with effective alpha 23.6197

### Third weakest link

# there is only one option, it must be the root node, but we can compute its
# effective alpha 

# Pruning the "root" node
n = dim(mydata)[1]
y = mydata$lpsa

Rnode = sum((y - mean(y))^2)

yHat = matrix(0, nrow = n)
yHat[mydata$lcavol < 2.462] = 2.1227
yHat[mydata$lcavol >= 2.462] = 3.7654

Rbranch = sum((y - yHat)^2)
(Rnode - Rbranch)

# effective alpha is 44.40128

alpha = c(4.4271, 23.6196, 44.40128)
fit$cptable

# cp is the effective alpha divided by the sum of squares of Y
R0 = sum((mydata$lpsa - mean(mydata$lpsa))^2)
alpha / R0

# In the fourth row, the value cp = 0.01 is because 
# that is the default minimum cp of the function. 
# It should actually be 0 (and we can see that by modifying 
# the minimum cp using rpart.control)

fit = rpart(lpsa ~ lcavol + lweight, data = mydata, 
            control = rpart.control(maxdepth = 2, cp = 0))
fit$cptable


pruned_fit = prune.rpart(fit, 0.01)
plot(pruned_fit)
text(pruned_fit)



###
# Prune the tree
###

# Set up cross-validation
n = dim(mydata)[1]
nFolds = 5
n_per_fold = floor(n / nFolds)

set.seed(0)
index = sample(c(1:n))
folds = list()
for(i in 1:(nFolds - 1)){
    folds[[i]] = index[c(((i - 1)*(n_per_fold) + 1):(i*n_per_fold))]
}
folds[[nFolds]] = index[c(((nFolds - 1)*n_per_fold + 1):n)]

fit$cptable
# Setup cp values
cp_values = c(0.015, 0.1, 0.25, 0.6)
n_cp_values = length(cp_values)

mse = matrix(0, nrow = nFolds, ncol = n_cp_values)

for(fold in 1:nFolds){
    
    # Define training / testing dataframes based on fold
    training_data = mydata[-folds[[fold]],]
    testing_data = mydata[folds[[fold]],]
    
    n_training = dim(training_data)[1]
    n_testing = dim(testing_data)[1]
    
    testingY = testing_data$lpsa
    
    tree = rpart(lpsa ~ lcavol + lweight, data = training_data, 
                control = rpart.control(maxdepth = 2, cp = 0))
    
    for(i in 1:n_cp_values){
        cp = cp_values[i]
        pruned_tree = prune(tree, cp)
        yHat = predict(pruned_tree, testing_data)
        mse[fold, i] = mean((yHat - testingY)^2)
    }
}

mse = apply(mse, 2, mean)    
plot(cp_values, mse, lwd = 2)

cp = cp_values[which.min(mse)]
cp

final_tree = prune(fit, cp)
plot(final_tree)
text(final_tree)






#############
# Growing a Regression Tree
#############

require(rpart)

mydata = read.csv('prostate-data.csv')

# Grow decision tree
# use rpart.control to set the growth parameters
fit = rpart(lpsa ~ lcavol + lweight, data = mydata, 
            control = rpart.control(cp = 0, minsplit = 9))
plot(fit)
text(fit, cex = 1.25)

###
# Prune the tree
###

# Set up cross-validation
n = dim(mydata)[1]
nFolds = 5
n_per_fold = floor(n / nFolds)

set.seed(0)
index = sample(c(1:n))
folds = list()
for(i in 1:(nFolds - 1)){
  folds[[i]] = index[c(((i - 1)*(n_per_fold) + 1):(i*n_per_fold))]
}
folds[[nFolds]] = index[c(((nFolds - 1)*n_per_fold + 1):n)]


# Setup cp values
cp_values = c(fit$cptable[1,1] * 1.1, (fit$cptable[c(1:17),1] + fit$cptable[c(2:18),1]) / 2)
n_cp_values = length(cp_values)

mse = matrix(0, nrow = nFolds, ncol = n_cp_values)

for(fold in 1:nFolds){
  
  # Define training / testing dataframes based on fold
  training_data = mydata[-folds[[fold]],]
  testing_data = mydata[folds[[fold]],]
  
  n_training = dim(training_data)[1]
  n_testing = dim(testing_data)[1]
  
  testingY = testing_data$lpsa
  
  tree = rpart(lpsa ~ lcavol + lweight, data = training_data, 
               control = rpart.control(cp = 0, minsplit = 9))
  
  for(i in 1:n_cp_values){
    cp = cp_values[i]
    pruned_tree = prune(tree, cp)
    yHat = predict(pruned_tree, testing_data)
    mse[fold, i] = mean((yHat - testingY)^2)
  }
}

mse = apply(mse, 2, mean)    
plot(cp_values, mse, lwd = 2)

cp = cp_values[which.min(mse)]
cp

final_tree = prune(fit, cp)
plot(final_tree)
text(final_tree)






####
# Decision Tree for predicting chd
####

require(rpart)

mydata = read.csv('SAheart-data.csv', header=T)
mydata$chd = factor(mydata$chd)

head(mydata)

mydata0 = mydata[mydata$chd == 0,]
n0 = dim(mydata0)[1]

mydata1 = mydata[mydata$chd == 1,]
n1 = dim(mydata1)[1]

nFolds = 10
n0_per_fold = floor(n0 / nFolds)
n1_per_fold = floor(n1 / nFolds)
folds0 = list()
folds1 = list()

set.seed(0)
shuffled_index0 = sample(c(1:n0))
shuffled_index1 = sample(c(1:n1))
for(fold in c(1:(nFolds - 1))){
    folds0[[fold]] = shuffled_index0[c((1 + (fold - 1) * n0_per_fold):(fold * n0_per_fold))]
    folds1[[fold]] = shuffled_index1[c((1 + (fold - 1) * n1_per_fold):(fold * n1_per_fold))]
}  
folds0[[nFolds]] = shuffled_index0[c((1 + (nFolds - 1) * n0_per_fold):n0)]
folds1[[nFolds]] = shuffled_index1[c((1 + (nFolds - 1) * n1_per_fold):n1)]



# Build the entire tree using gini
fit = rpart(chd ~ ., data = mydata, method = 'class', 
            parms = list(split = 'gini'))
fit
plot(fit)
text(fit)

fit$cp[,1]
cp_values = c(0.011, 0.015, 0.022, 0.04, 0.08, 1.1, 1.5)
n_cp_values = length(cp_values) 

accuracy = matrix(0, nrow = nFolds, ncol = n_cp_values)

for(fold in 1:nFolds){
    
    # Define training / testing dataframes based on fold
    training_data = rbind(mydata0[-folds0[[fold]],], mydata1[-folds1[[fold]],])
    testing_data = rbind(mydata0[folds0[[fold]],], mydata1[folds1[[fold]],])
    
    n_training = dim(training_data)[1]
    n_testing = dim(testing_data)[1]
    
    testingY = testing_data$chd
    
    tree = rpart(chd ~ ., data = training_data, method = 'class', 
                 parms = list(split = 'gini'))
    
    for(i in 1:n_cp_values){
        cp = cp_values[i]
        pruned_tree = prune(tree, cp)
        yHat = predict(pruned_tree, testing_data, type = 'class')
        accuracy[fold, i] = mean(yHat == testingY)
    }
}

accuracy = apply(accuracy, 2, mean)
plot(cp_values, accuracy, lwd = 2)

cp = cp_values[which.max(accuracy)]

final_fit = prune.rpart(fit, cp)
plot(final_fit)
text(final_fit)



