########## Decision Trees ##########
require(rpart)
make_folds = function(Y, nFolds, stratified = FALSE, seed = 0){
  # K-Fold cross validation
  # Input:
  #   Y (either sample size, or vector of outputs)
  #   stratified (boolean): whether the folds should 
  #     be stratified. Requires Y to be a vector of outputs
  # Output: list of vectors of fold indices
  set.seed(seed)
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

mydata = read.csv('prostate-data.csv')

# Regression Treee
# Grow decision tree
fit = rpart(lpsa ~ lcavol + lweight, data = mydata, 
            control = rpart.control(maxdepth = 2))
plot(fit)
text(fit, cex = 1.25)
fit


###
# Prune tree
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

### Classification tree ###
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

########## support vector machines ##########
require(e1071)

# kernel = linear to make the SVM and SVC
# cost = 1e100 makes the SVC return the seperable hyperplane solution
fit = svm(y ~ x1 + x2, data=mydata, scale = FALSE, type = 'C',
          kernel = 'linear', cost = 1e100)

# fit$coefs is the value of y_i * alpha_i for all alpha_i != 0
fit$coefs
sum(fit$coefs)

# which data points are support vectors
fit$index


fit$coefs / mydata$y[fit$index]

# note that the alpha/y term is the wrong sign...
# this is because, for some reason, svm() sometimes reverses the sign of 
# the outputs.
fit$coefs / (-mydata$y[fit$index])

# Support Vectors
fit$SV

# Estimate B
bHat <- t(fit$coefs) %*% fit$SV

# Solve for b0 using SV[1]
x = as.matrix(c(mydata$x1[fit$index[1]], mydata$x2[fit$index[1]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[1]]) / fit$coefs[1]
b0

# Solve for b0 using SV[2]
x = as.matrix(c(mydata$x1[fit$index[2]], mydata$x2[fit$index[2]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[2]]) / fit$coefs[1]
b0

# Solve for b0 using SV[3]
x = as.matrix(c(mydata$x1[fit$index[3]], mydata$x2[fit$index[3]]), nrow = 1)
b0 = -(fit$coefs[1] * (bHat %*% x) + fit$coefs[1] / mydata$y[fit$index[3]]) / fit$coefs[1]
b0


# f(x) = b0 + xB = b0 + b1x1 + b2x2 = 0
# b0 + b1x1 + b2x2 = 0
# x2 = -(b1x1 + b0)/b2

plot(mydata$x1[mydata$y == -1], mydata$x2[mydata$y == -1], xlim = x1range, ylim = x2range, 
     bty = 'n', col = 'orange', lwd = 2, xlab = 'x1', ylab = 'x2')

points(mydata$x1[mydata$y == 1], mydata$x2[mydata$y == 1], col = 'blue', lwd = 2)

points(mydata[fit$index,c(2,3)],col='black',cex=3, lwd = 2)

abline(a=-b0/bHat[2], b=-bHat[1]/bHat[2], col="black", lwd = 2)











