data = read.csv("simple-classification-data.csv")
head(data)

# Part a
# Define the regions as a list of functions
regions <- list(
  R1 = function(x1, x2) x1 <= 0.5 & x2 <= 1,
  R2 = function(x1, x2) x1 <= 0.5 & x2 > 1,
  R3 = function(x1, x2) x1 > 0.5 & x2 <= 1,
  R4 = function(x1, x2) x1 > 0.5 & x2 > 1
)

# Function to find region centers
find_region_center <- function(region_func, x1_range, x2_range) {
  points <- expand.grid(x1 = x1_range, x2 = x2_range)
  in_region <- apply(points, 1, function(p) region_func(p[1], p[2]))
  region_points <- points[in_region, ]
  return(c(mean(region_points$x1), mean(region_points$x2)))
}

# Create the plot
plot(c(0, 1), c(0, 2), type = "n", xlab = "x1", ylab = "x2",
     main = "Two-Dimensional Spline Regions")

# Determine boundaries from region definitions
# Boundary at x1 = 0.5 (where R1/R2 changes to R3/R4)
x2_vals <- seq(0, 2, by = 0.01)
lines(rep(0.5, length(x2_vals)), x2_vals, lty = 2)

# Boundary at x2 = 1 (where R1/R3 changes to R2/R4)
x1_vals <- seq(0, 1, by = 0.01)
lines(x1_vals, rep(1, length(x1_vals)), lty = 2)

# Find centers and add labels for each region
# Define sampling ranges for each region
region_ranges <- list(
  R1 = list(x1 = seq(0, 0.5, by = 0.1), x2 = seq(0, 1, by = 0.1)),
  R2 = list(x1 = seq(0, 0.5, by = 0.1), x2 = seq(1, 2, by = 0.1)),
  R3 = list(x1 = seq(0.5, 1, by = 0.1), x2 = seq(0, 1, by = 0.1)),
  R4 = list(x1 = seq(0.5, 1, by = 0.1), x2 = seq(1, 2, by = 0.1))
)

# Add labels for each region
for (i in 1:4) {
  region_name <- paste0("R", i)
  center <- find_region_center(
    regions[[region_name]], 
    region_ranges[[region_name]]$x1, 
    region_ranges[[region_name]]$x2
  )
  text(center[1], center[2], region_name)
}

# Part b
get_region <- function(x1, x2) {
  for (i in 1:4) {
    region_name <- paste0("R", i)
    if (regions[[region_name]](x1, x2)) {
      return(i)
    }
  }
  return(NA)  # Should not happen if regions cover all space
}

# Assign regions to all data points
data$region <- mapply(get_region, data$x1, data$x2)

# Fit piecewise constant model
# For each region, use the majority class as the prediction
region_models <- list()
for (i in 1:4) {
  region_data <- subset(data, region == i)
  if (nrow(region_data) > 0) {
    # Count occurrences of each class in this region
    class_counts <- table(region_data$y)
    # Use majority class as the model for this region
    majority_class <- as.numeric(names(class_counts)[which.max(class_counts)])
    region_models[[i]] <- majority_class
  } else {
    # If no data in a region, use overall majority class
    region_models[[i]] <- as.numeric(names(which.max(table(data$y))))
  }
}

# Make predictions for all data points
predict_y <- function(x1, x2) {
  region <- get_region(x1, x2)
  return(region_models[[region]])
}

data$y_pred <- mapply(predict_y, data$x1, data$x2)

# Calculate training accuracy
accuracy <- mean(data$y == data$y_pred)
cat("Training accuracy:", round(accuracy * 100, 2), "%\n")

# Print the piecewise constant model
cat("\nPiecewise Constant Model:\n")
for (i in 1:4) {
  cat("Region R", i, ": y = ", region_models[[i]], "\n", sep = "")
}


# Load the dataset if not already loaded
if (!exists("data")) {
  data = read.csv("simple-classification-data.csv")
}

# Function to create basis functions for the piecewise linear model
# We need basis functions that allow for slope changes at x1 = 0.5 and x2 = 1
create_basis_matrix = function(x1, x2) {
  # Create basis matrix
  # 1. Constant term
  # 2. x1 term
  # 3. x2 term
  # 4. (x1 - 0.5)+ term (allows slope change at x1 = 0.5)
  # 5. (x2 - 1)+ term (allows slope change at x2 = 1)
  
  # ReLU function (x)+ = max(0, x)
  relu = function(x) pmax(0, x)
  
  # Create the basis matrix
  X = cbind(
    1,             # Constant term
    x1,            # Linear term for x1
    x2,            # Linear term for x2
    relu(x1 - 0.5), # ReLU term for x1 at knot 0.5
    relu(x2 - 1)    # ReLU term for x2 at knot 1
  )
  
  colnames(X) = c("Constant", "x1", "x2", "x1_knot", "x2_knot")
  return(X)
}

# Create the design matrix for our data
X = create_basis_matrix(data$x1, data$x2)

# Fit logistic regression model
# For classification, we use logistic regression to fit a piecewise linear model
model = glm(y ~ 0 + ., data = data.frame(y = data$y, X), family = binomial)

# Print model coefficients
cat("Piecewise Linear Model Coefficients:\n")
print(coef(model))

# Make predictions
logits = predict(model, data.frame(X), type = "link")
probs = 1 / (1 + exp(-logits))
y_pred = ifelse(probs > 0.5, 1, 0)

# Calculate training accuracy
accuracy = mean(data$y == y_pred)
cat("\nTraining accuracy:", round(accuracy * 100, 2), "%\n")

# Visualize the decision boundary of the piecewise linear model
# Create a grid of points
grid_size = 100
x1_grid = seq(0, 1, length.out = grid_size)
x2_grid = seq(0, 2, length.out = grid_size)
grid_points = expand.grid(x1 = x1_grid, x2 = x2_grid)

# Create basis matrix for grid points
X_grid = create_basis_matrix(grid_points$x1, grid_points$x2)

# Predict probabilities for grid points
grid_logits = predict(model, data.frame(X_grid), type = "link")
grid_probs = 1 / (1 + exp(-grid_logits))
grid_pred = ifelse(grid_probs > 0.5, 1, 0)

# Reshape predictions for contour plot
z = matrix(grid_pred, nrow = grid_size, ncol = grid_size)

# Plot the data and decision boundary
plot(data$x1, data$x2, col = ifelse(data$y == 1, "blue", "red"), 
     pch = ifelse(y_pred == data$y, 16, 4),
     main = "Piecewise Linear Model with Decision Boundary",
     xlab = "x1", ylab = "x2")

# Add decision boundary contour
contour(x1_grid, x2_grid, z, levels = 0.5, add = TRUE, lwd = 2)

# Add knot lines
abline(v = 0.5, lty = 2)
abline(h = 1, lty = 2)

# Add legend
legend("topright", 
       legend = c("Class 0", "Class 1", "Correct Prediction", "Incorrect Prediction"),
       col = c("red", "blue", "black", "black"),
       pch = c(16, 16, 16, 4))

# Evaluate the model at specific points to verify continuity
evaluate_model = function(x1, x2) {
  X_point = create_basis_matrix(x1, x2)
  logit = sum(coef(model) * X_point)
  prob = 1 / (1 + exp(-logit))
  return(prob)
}

# Check continuity at x1 = 0.5 by evaluating points slightly to the left and right
x1_left = 0.499
x1_right = 0.501
x2_test = 0.5

prob_left = evaluate_model(x1_left, x2_test)
prob_right = evaluate_model(x1_right, x2_test)

cat("\nContinuity check at x1 = 0.5:\n")
cat("Probability at x1 =", x1_left, ":", prob_left, "\n")
cat("Probability at x1 =", x1_right, ":", prob_right, "\n")
cat("Difference:", abs(prob_right - prob_left), "\n")

# Similarly for x2 = 1
x1_test = 0.5
x2_below = 0.999
x2_above = 1.001

prob_below = evaluate_model(x1_test, x2_below)
prob_above = evaluate_model(x1_test, x2_above)

cat("\nContinuity check at x2 = 1:\n")
cat("Probability at x2 =", x2_below, ":", prob_below, "\n")
cat("Probability at x2 =", x2_above, ":", prob_above, "\n")
cat("Difference:", abs(prob_above - prob_below), "\n")






