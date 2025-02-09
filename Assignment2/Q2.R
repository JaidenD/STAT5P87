createFolds <- function(y, k, stratified = FALSE, seed = 0) {
  # Set the random seed
  set.seed(seed)
  
  # If y is a single number, treat it as sample size and create an index vector.
  if (length(y) == 1 && is.numeric(y)) {
    n <- y
    if (stratified) {
      warning("Stratification is only valid for categorical outputs. Using unstratified folds.")
      stratified <- FALSE
    }
    y <- seq_len(n)
  } else {
    n <- length(y)
  }
  
  folds <- vector("list", k)
  
  if (!stratified) {
    # Unstratified: Simply randomize indices and assign to folds.
    indices <- sample(n)
    
    # Compute fold sizes: most folds will have floor(n/k) observations;
    # the first few folds will get an extra one if n is not divisible by k.
    fold_sizes <- rep(floor(n/k), k)
    remainder <- n - sum(fold_sizes)
    if (remainder > 0) {
      fold_sizes[1:remainder] <- fold_sizes[1:remainder] + 1
    }
    
    start_index <- 1
    for (i in 1:k) {
      end_index <- start_index + fold_sizes[i] - 1
      folds[[i]] <- indices[start_index:end_index]
      start_index <- end_index + 1
    }
  } else {
    # Stratified: The fold structure will preserve class proportions.
    # First, ensure that y is a factor.
    if (!is.factor(y)) {
      y <- as.factor(y)
    }
    # Initialize each fold as an empty vector.
    for (i in 1:k) {
      folds[[i]] <- c()
    }
    
    # For each level (class), split the indices of that class into k roughly equal parts.
    for (lvl in levels(y)) {
      lvl_indices <- which(y == lvl)
      lvl_indices <- sample(lvl_indices)  # Randomize the indices for this level.
      n_lvl <- length(lvl_indices)
      
      # Compute fold sizes for this class.
      fold_sizes <- rep(floor(n_lvl/k), k)
      remainder <- n_lvl - sum(fold_sizes)
      if (remainder > 0) {
        fold_sizes[1:remainder] <- fold_sizes[1:remainder] + 1
      }
      
      start_index <- 1
      for (i in 1:k) {
        end_index <- start_index + fold_sizes[i] - 1
        folds[[i]] <- c(folds[[i]], lvl_indices[start_index:end_index])
        start_index <- end_index + 1
      }
    }
  }
  
  return(folds)
}

kfold(10, 2, FALSE, )
  # 
