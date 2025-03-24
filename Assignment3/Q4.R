data = read.csv("SAheart-data.csv")

# Cubic function for spline construction
d_function = function(x, knot) {
  pmax(0, x - knot)^3
}

# Build natural spline basis
build_ns_basis = function(x, ns_knots, knot_min, knot_max, varname = "x") {
  n_knots = length(ns_knots)
  ns_mat  = matrix(0, nrow = length(x), ncol = n_knots)
  for (j in 1:n_knots) {
    alpha = (knot_max - ns_knots[j]) / (knot_max - knot_min)
    beta  = (ns_knots[j] - knot_min) / (knot_max - knot_min)
    ns_mat[, j] = d_function(x, ns_knots[j]) -
      alpha * d_function(x, knot_max) -
      beta  * d_function(x, knot_min)
  }
  colnames(ns_mat) = paste0(varname, "_ns", 1:n_knots)
  return(ns_mat)
}

# Candidate predictors and precompute their spline bases
vars = c("sbp", "tobacco", "ldl", "age")
spline_bases = list()
for (v in vars) {
  x = data[[v]]
  knot_min = min(x)
  knot_max = max(x)
  # 5 equally spaced knots, 3 interior, 2 boundary
  ns_knots = seq(knot_min, knot_max, length.out = 5)[2:4]
  spline_bases[[v]] = build_ns_basis(x, ns_knots, knot_min, knot_max, varname = v)
}

# Forward selection by AIC for logistic regression
selected_vars = c()
current_aic   = Inf
best_model    = NULL
improvement   = TRUE

while (improvement && length(selected_vars) < length(vars)) {
  improvement = FALSE
  best_aic_step = current_aic
  best_candidate = NULL
  
  # Try adding each variable not yet selected
  for (candidate in setdiff(vars, selected_vars)) {
    df_temp = data.frame(chd = data$chd)
    if (length(selected_vars) > 0) {
      for (sv in selected_vars) {
        df_temp = cbind(df_temp, spline_bases[[sv]])
      }
    }
    df_temp = cbind(df_temp, spline_bases[[candidate]])
    
    model_temp = glm(chd ~ ., data = df_temp, family = binomial)
    aic_temp = AIC(model_temp)
    if (aic_temp < best_aic_step) {
      best_aic_step = aic_temp
      best_candidate = candidate
    }
  }
  
  if (!is.null(best_candidate)) {
    selected_vars = c(selected_vars, best_candidate)
    current_aic = best_aic_step
    improvement = TRUE
    
    df_final = data.frame(chd = data$chd)
    for (sv in selected_vars) {
      df_final = cbind(df_final, spline_bases[[sv]])
    }
    best_model = glm(chd ~ ., data = df_final, family = binomial)
  }
}

# Report selected variables, final AIC, and training accuracy
pred_probs = predict(best_model, type = "response")
pred_class = ifelse(pred_probs > 0.5, 1, 0)
train_acc  = mean(pred_class == data$chd)

selected_vars # "age"     "ldl"     "tobacco"
AIC(best_model) # 521.164
train_acc # 0.7142857
