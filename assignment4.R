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
  roc_obj <- roc(data$v2, pred_probs)
  auc_value <- auc(roc_obj)

  return(list(AUC = auc_value, ROC = roc_obj))
}

exercise41 <- function(data) {
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

  # Fit the logistic regression model (if not already done)
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
  cutoffs <- c(0.5, 0.3, 0.7, 0.25, 0.4, 0.2) # Cutoffs of 0.5, 0.3, and 0.7

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
}

main <- function() {
  # Load the necessary libraries
  library(pROC)
  library(rpart)
  library(rpart.plot)

  # Load the dataset
  data_original <- read.csv("data_ca4.csv")
  data <- data_original

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
  categorical_cols <- c("v4", "v5", "v6", "v7", "v8", "v9", "v10", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21")
  data[categorical_cols] <- lapply(data[categorical_cols], as.factor)

  ## Exercise 4:1.1
  # exercise41(data)

  # Exercise 4:2.1

  # Set working directory and read data (replace "path_to_file" with actual path)
  # TODO To Preprocess Or Not To Preprocess that is the question
  data4 <- data

  # Fit and plot decision trees with different cp values
  cp_values <- c(0.05, 0.01, 0.001)

  for (cp in cp_values) {
    cat("Fitting tree with cp =", cp, "\n")
    tree <- rpart(v2 ~ ., data = data4, method = "class", parms = list(split = "gini"), cp = cp)
    rpart.plot(tree, main = paste("Decision Tree (Gini) with cp =", cp), digits = 3)
  }

  tree3 <- rpart(v2 ~ ., method = "class", data = data4, parms = list(split = "information"), cp = 0.01)
  rpart.plot(tree3, digits = 3, main = "Decision Tree (Information, cp=0.01)")

  # Choose a 'good' tree
  # Assess model simplicity and interpretability. The 'cp' value that results in a manageable number of splits
  # and accurate predictions is often the best choice.

  # Example: Optimal tree based on experimentation
  optimal_tree <- rpart(v2 ~ ., method = "class", data = data4, parms = list(split = "information"), cp = 0.05)
  rpart.plot(optimal_tree, digits = 3, main = "Optimal Decision Tree (Information, cp=0.05)")

  # Fit a decision tree model
  tree_model <- rpart(v2 ~ ., data = data4, method = "class", parms = list(split = "gini"), cp = 0.1)
  tree_model <- rpart(v2 ~ ., data = data4, method = "class", parms = list(split = "information"), cp = 0.001)

  # Plot the tree
  rpart.plot(tree_model, main = "Decision Tree (Initial Model)", digits = 3)
  
  ## Exercise 4:2.2
  
  # Predict probabilities for the decision tree
  tree_probs <- predict(tree_model, type = "prob")[, 2]  # Probability for class "Not Survive" (v2 = 1)
  
  # ROC and AUC for Decision Tree
  tree_roc <- roc(data4$v2, tree_probs)
  print(tree_roc)
  plot(tree_roc, main = "ROC Curve for Decision Tree Model", col = "blue")
  
  # Logistic Regression Model
  logistic_model <- glm(v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17, family = binomial, data = data)
  
  # Predict probabilities for logistic regression
  logistic_probs <- predict(logistic_model, type = "response")
  
  # ROC and AUC for Logistic Regression
  logistic_roc <- roc(data4$v2, logistic_probs)
  print(logistic_roc)
  plot(logistic_roc, add = TRUE, col = "red")  # Add ROC curve for logistic regression
  
  # Compare AUC
  cat("AUC for Decision Tree:", auc(tree_roc), "\n")
  cat("AUC for Logistic Regression:", auc(logistic_roc), "\n")
  
  
  # Initialize a vector to store LOOCV predictions
  loocv_probs <- numeric(nrow(data4))
  
  # Perform LOOCV
  for (i in 1:nrow(data4)) {
    # Training data (exclude the i-th observation)
    train_data <- data4[-i, ]
    # Test data (only the i-th observation)
    test_data <- data4[i, , drop = FALSE]
    
    # Fit the decision tree model on training data
    loocv_tree <- rpart(v2 ~ ., method = "class", data = train_data, parms = list(split = "information"), cp = 0.001)
    
    # Predict the probability for the test observation
    loocv_probs[i] <- predict(loocv_tree, test_data, type = "prob")[, 2]
  }
  
  # Calculate the LOOCV-corrected AUC
  loocv_roc <- roc(data4$v2, loocv_probs)
  print(loocv_roc)
  plot(loocv_roc, main = "LOOCV-Corrected ROC Curve for Decision Tree", col = "blue")
  
  logistic_roc_loocv <- loocv_auc(data4, v2 ~ v21 + v14 + v3 + v7 + v11 + v18 + v17)
  plot(logistic_roc_loocv$ROC, add = TRUE, col = "red")
  
  # Print the LOOCV-corrected AUC
  cat("LOOCV-Corrected AUC for Decision Tree:", auc(loocv_roc), "\n")
  cat("LOOCV-Corrected AUC for Logistic Regression:", logistic_roc_loocv$AUC, "\n")

  return(list(
    data4 = data4,
    tree_model = tree_model
  ))
}

ass <- main()
