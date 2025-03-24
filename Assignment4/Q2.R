require(e1071)

data = read.csv("SAheart-data.csv")

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

y = data$chd

folds = make_folds(y, 5, stratified = TRUE)
degrees = 1:5
cv_error = numeric(length(degrees))

for (j in degrees) {
  print(j)
  d = degrees[j]
  fold_errors = numeric(length(folds))
  
  for (i in seq_along(folds)) {
    print(i)
    test_idx   = folds[[i]]
    train_data = data[-test_idx, ]
    test_data  = data[test_idx, ]
    
    model = svm(
      chd ~ ., 
      data   = train_data,
      kernel = "polynomial",
      degree = d,
      gamma  = 1,
      scale  = TRUE,
      type   = "C"
    )
    
    # Predict on test fold
    predictions = predict(model, newdata = test_data)
    
    # Calculate classification error for fold
    fold_errors[i] = mean(predictions != test_data$chd)
  }
  
  # Average error across the 5 folds for this degree d
  cv_error[j] = mean(fold_errors)
}

best_degree = degrees[which.min(cv_error)] # best_degree = 1

final_model <- svm(
  chd ~ .,
  data   = data,
  kernel = "polynomial",
  degree = best_degree,
  gamma  = 1,
  scale  = FALSE,
  type   = "C"
)

summary(final_model)



