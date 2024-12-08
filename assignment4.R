loocv_auc <- function(data, formula) {
  n <- nrow(data)
  pred_probs <- numeric(n) # Vector to store predicted probabilities

  # Loop through each observation
  for (i in 1:n) {
    # Create training and test sets
    train_data <- data[-i, ] # All but the ith observation
    test_data <- data[i, , drop = FALSE] # Only the ith observation

    # Fit the model on the training set
    model <- glm(formula, family = binomial, data = train_data)

    # Predict for the left-out observation
    pred_probs[i] <- predict(model, newdata = test_data, type = "response")
  }

  # Calculate the ROC and AUC
  roc_obj <- roc(data$v2, pred_probs, levels = c(0, 1), direction = "<")
  auc_value <- auc(roc_obj)

  return(list(AUC = auc_value, ROC = roc_obj))
}

exercise41 <- function(data) {
  ## Exercise 4:1.1
  # Empty Model - AIC:212
  m1 <- glm(v2 ~ 1, family = binomial, data = data) # Empty model (only intercept)

  # All predictor variables - AIC:166.72
  full_model <- glm(v2 ~ ., data = data, family = binomial)
  summary(full_model)

  # v2 ~ v21 + v14 + v3 + v7 + v1 + v11 + v18 + v17
  mstep1_AIC <- step(m1, direction = "forward", scope = list(upper = full_model), trace = TRUE)
  # mstep2_AIC <- step(full_model, direction="backward", trace = FALSE)
  # v2 ~ v1 + v3 + v7 + v11 + v14 + v17 + v18 + v21
  # AIC = 144
  # mstep3_AIC <- step(full_model, direction="both", trace = FALSE)
  summary(mstep1_AIC)
  mstep1_BIC <- step(m1,
    direction = "forward", scope = list(upper = full_model),
    k = log(nrow(data)), trace = TRUE
  )

  final_model <- glm(v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17, family = binomial, data = data)

  # Compute Odds and CI for final model's coefficients
  # TODO When writing RMD, interpret v211 and similar variable with its context - line by line
  exp_coef <- exp(coef(final_model))
  conf_intervals <- exp(confint(final_model))
  odds_ratios <- data.frame(
    Variable = names(exp_coef),
    Odds_Ratio = exp_coef,
    Lower_CI = conf_intervals[, 1],
    Upper_CI = conf_intervals[, 2]
  )
  print(odds_ratios)

  ## Exercise 4:1.2
  # TODO: RMD explaining why other statistical tests wont work
  # Load the necessary library
  library(ResourceSelection)

  # Obtain predicted probabilities
  predicted_probs <- predict(final_model, type = "response")

  # Perform the Hosmer-Lemeshow goodness-of-fit test
  hoslem_test <- hoslem.test(data$v2, predicted_probs, g = 10) # 'g' specifies number of groups

  # Display the test results
  print(hoslem_test)

  # TODO: RMD explaining Hosmer-Lemeshow interpretation

  ## Exercise 4:1.3

  # Generate predicted probabilities from the logistic regression model
  predicted_probs <- predict(final_model, type = "response")

  # Define cutoff values
  cutoffs <- c(0.5, 0.75, 0.25) # Cutoffs of 0.5, 0.3, and 0.7

  # Create an empty list to store confusion matrices and metrics
  results <- list()

  # Loop through each cutoff to calculate metrics
  for (cutoff in cutoffs) {
    # Convert probabilities to binary predictions based on the cutoff
    predicted_class <- ifelse(predicted_probs > cutoff, 1, 0)

    # Generate the confusion matrix
    confusion_matrix <- table(Actual = data$v2, Predicted = predicted_class)

    # Calculate metrics
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ]) # TP / (TP + FN)
    specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ]) # TN / (TN + FP)

    # Store the results
    results[[as.character(cutoff)]] <- list(
      Confusion_Matrix = confusion_matrix,
      Accuracy = accuracy,
      Sensitivity = sensitivity,
      Specificity = specificity
    )
  }
  # Print metrics for each cutoff
  for (cutoff in cutoffs) {
    cat("\nCutoff:", cutoff, "\n")
    print(results[[as.character(cutoff)]]$Confusion_Matrix)
    cat("Accuracy:", results[[as.character(cutoff)]]$Accuracy, "\n")
    cat("Sensitivity:", results[[as.character(cutoff)]]$Sensitivity, "\n")
    cat("Specificity:", results[[as.character(cutoff)]]$Specificity, "\n")
  }

  # Exercise 4:1.4

  # Fit the models
  full_model <- glm(v2 ~ ., family = binomial, data = data) # Full
  model_1 <- glm(v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17, family = binomial, data = data) # AIC
  # model_1 <- glm(v2 ~ v21 + v14 + v3, family = binomial, data = data)  # Reduced model 1
  model_2 <- glm(v2 ~ v21 + v14 + v3 + v7, family = binomial, data = data) # BIC

  # Generate predicted probabilities for each model
  pred_full <- predict(full_model, type = "response")
  pred_model_1 <- predict(model_1, type = "response")
  pred_model_2 <- predict(model_2, type = "response")

  # Create ROC objects
  roc_full <- roc(data$v2, pred_full)
  roc_model_1 <- roc(data$v2, pred_model_1)
  roc_model_2 <- roc(data$v2, pred_model_2)

  # Plot the ROC curves
  plot(roc_full, col = "blue", legacy.axes = TRUE, main = "ROC Curves for Logistic Models")
  lines(roc_model_1, col = "red") # Add Model 1 ROC curve
  lines(roc_model_2, col = "green") # Add Model 2 ROC curve

  # Add legend
  legend("bottomright",
    legend = c("Full Model", "Model 1", "Model 2"),
    col = c("blue", "red", "green"), lwd = 2
  )
  cat("AUC for Full Model:", auc(roc_full), "\n")
  cat("AUC for Model 1:", auc(roc_model_1), "\n")
  cat("AUC for Model 2:", auc(roc_model_2), "\n")

  ## Exercise 4:1.5

  # Full model
  full_formula <- v2 ~ .
  loocv_full <- loocv_auc(data, full_formula)

  # Model 1
  model_1_formula <- v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17
  loocv_model_1 <- loocv_auc(data, model_1_formula)

  # Model 2
  model_2_formula <- v2 ~ v21 + v14 + v3 + v7
  loocv_model_2 <- loocv_auc(data, model_2_formula)

  cat("LOOCV AUC for Full Model:", loocv_full$AUC, "\n")
  cat("LOOCV AUC for Model 1:", loocv_model_1$AUC, "\n")
  cat("LOOCV AUC for Model 2:", loocv_model_2$AUC, "\n")

  # Plot the ROC curves
  plot(loocv_full$ROC, col = "blue", legacy.axes = TRUE, main = "LOOCV ROC Curves")
  lines(loocv_model_1$ROC, col = "red")
  lines(loocv_model_2$ROC, col = "green")

  # Add a legend
  legend("bottomright",
    legend = c("Full Model", "Model 1", "Model 2"),
    col = c("blue", "red", "green"), lwd = 2
  )
  
  return(list(
    mstep1_AIC = mstep1_AIC,
    mstep1_BIC = mstep1_BIC,
    final_model = final_model,
    odds_ratios = odds_ratios,
    hoslem_test = hoslem_test,
    cutoffs = cutoffs,
    results = results,
    roc_model_1 = roc_model_1,
    roc_model_2 = roc_model_2,
    roc_full = roc_full,
    #tree_roc = tree_roc,
    loocv_model_1 = loocv_model_1,
    loocv_model_2 = loocv_model_2,
    loocv_full = loocv_full 
    ))
}

exercise42 <- function(data) {
  # Exercise 4:2.1
  # We are relying on the preprocessed data and that is probably fine
  data4 <- data

  # Fit and plot decision trees with different cp values
  plot_decision_trees(data, 0.1)
  plot_decision_trees(data, 0.01)
  plot_decision_trees(data, 0.001)

  # Example: Optimal tree based on experimentation
  tm_good_cp <- 0.001
  tm_good_split <- "information"
  tm_good_split_name <- "Information gain"
  tm_good <- make_tree(data4, tm_good_split, tm_good_cp)
  rpart.plot(tm_good, main = paste(tm_good_split_name, "with cp =", tm_good_cp))

  ## Exercise 4:2.2
  # ROC of Decision Tree
  tree_probs <- predict(tm_good, type = "prob")[, 2]
  tree_roc <- roc(data4$v2, tree_probs, levels = c(0, 1), direction = "<")

  # ROC of Logistic Model
  logistic_model <- glm(v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17, family = binomial, data = data)
  logistic_probs <- predict(logistic_model, type = "response")
  logistic_roc <- roc(data4$v2, logistic_probs, levels = c(0, 1), direction = "<")

  # Plot comparison
  plot_tree_against_logistic(tree_roc, logistic_roc)

  ## Exercise 4:2.3

  # Initialize a vector to store LOOCV predictions
  loocv_probs <- numeric(nrow(data4))

  # Perform LOOCV
  for (i in 1:nrow(data4)) {
    # Training data (exclude the i-th observation)
    train_data <- data4[-i, ]
    # Test data (only the i-th observation)
    test_data <- data4[i, , drop = FALSE]

    # Fit the decision tree model on training data
    loocv_tree <- rpart(
      v2 ~ .,
      method = "class", data = train_data,
      parms = list(split = tm_good_split), cp = tm_good_cp
    )

    # Predict the probability for the test observation
    loocv_probs[i] <- predict(loocv_tree, test_data, type = "prob")[, 2]
  }

  # Calculate the LOOCV-corrected AUC
  tree_roc_loocv <- roc(data4$v2, loocv_probs, levels = c(0, 1), direction = "<")

  logistic_roc_loocv <- loocv_auc(
    data4,
    v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17
  )$ROC
  plot_tree_against_logistic(tree_roc_loocv, logistic_roc_loocv)

  return(list(
    data4 = data4,
    tm_good = tm_good,
    tm_good_cp = tm_good_cp,
    tm_good_split_name = tm_good_split_name,
    tree_roc = tree_roc,
    logistic_roc = logistic_roc,
    tree_roc_loocv = tree_roc_loocv,
    logistic_roc_loocv = logistic_roc_loocv
  ))
}

make_tree <- function(data, split_method, cp) {
  parms <- list(split = split_method)
  tm <- rpart(v2 ~ ., data = data, method = "class", parms = parms, cp = cp)
  return(tm)
}

plot_tree_against_logistic <- function(tree_roc, logistic_roc) {
  plot(tree_roc, main = "ROC Curve", col = "blue")
  plot(logistic_roc, add = TRUE, col = "red")
  legend("bottomright",
    legend = c("Decision tree", "Logistic model"),
    col = c("blue", "red"), lwd = 2
  )
}

plot_decision_trees <- function(data, cp) {
  tm_gini <- make_tree(data, "gini", cp)
  tm_info <- make_tree(data, "information", cp)
  par(mfrow = c(1, 2))
  rpart.plot(tm_gini, main = paste("Gini index, cp =", cp))
  rpart.plot(tm_info, main = paste("Information gain, cp =", cp))
  par(mfrow = c(1, 1))
}

preprocess <- function(data) {
  # RMD TODO - Explain Pre-processing using counts of the categories for ethnicities and consciousness level
  # Combine categories for Ethnicity (e.g., combine 2 and 3 into a single category)
  data$v5[data$v5 > 1] <- 0 # Combine categories 2 and 3 into 0 (other)

  # Combine categories for Consciousness level (e.g., combine 1 and 2 into a single category)
  data$v21[data$v21 > 1] <- 1 # Combine unconscious and coma into a single category

  # Remove the column named "v1"
  data <- data[, !names(data) %in% "v1"]

  # Check the modified dataset
  table(data$v5) # Verify the Ethnicity column changes
  table(data$v21) # Verify the Consciousness level changes

  # Optional: Ensure all categorical variables are factors
  # TODO: Check before and after converting to factors - See if it makes a difference
  categorical_cols <- c(
    "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v13",
    "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21"
  )
  data[categorical_cols] <- lapply(data[categorical_cols], as.factor)
  return(data)
}

main <- function() {
  # Load the necessary libraries
  library(pROC)
  library(rpart)
  library(rpart.plot)

  # Load the dataset
  data_original <- read.csv("data_ca4.csv")
  data <- preprocess(data_original)

  ## Exercise 4:1
  r41 <- exercise41(data)

  ## Exercise 4:2
  r42 <- exercise42(data)

  return(list(
    data_original = data_original,
    data = data,
    mstep1_AIC = r41$mstep1_AIC,
    mstep1_BIC = r41$mstep1_BIC,
    final_model = r41$final_model,
    odds_ratios = r41$odds_ratios,
    hoslem_test = r41$hoslem_test,
    cutoffs = r41$cutoffs,
    results = r41$results,
    roc_model_1 = r41$roc_model_1,
    roc_model_2 = r41$roc_model_2,
    roc_full = r41$roc_full,
    loocv_model_1 = r41$loocv_model_1,
    loocv_model_2 = r41$loocv_model_2,
    loocv_full = r41$loocv_full, 
    data4 = r42$data4,
    tm_good = r42$tm_good,
    tm_good_cp = r42$tm_good_cp,
    tm_good_split_name = r42$tm_good_split_name,
    tree_roc = r42$tree_roc,
    logistic_roc4 = r42$logistic_roc,
    tree_roc_loocv = r42$tree_roc_loocv,
    logistic_roc_loocv4 = r42$logistic_roc_loocv
  ))
}

ass <- main()
# ass <- list()
