plot_risk <- function(log_dose, risk) {
  # Plot risk against log(dose)
  plot(log_dose, risk,
    type = "b", col = "blue", pch = 19,
    xlab = "Log(Dose)", ylab = "Risk of Tumor",
    main = "Risk of Tumor vs. Log(Dose)"
  )
}

plot_odds <- function(log_dose, log_odds) {
  # Plot log-odds against log(dose)
  plot(log_dose, log_odds,
    type = "b", col = "red", pch = 19,
    xlab = "Log(Dose)", ylab = "Log-Odds of Tumor",
    main = "Log-Odds of Tumor vs. Log(Dose)"
  )
}

# Function to fit logistic regression and extract variances for scaled data
scale_and_fit <- function(scale_factor, log_dose, tumor, no_tumor) {
  # Scale the counts
  tumor_scaled <- tumor * scale_factor
  no_tumor_scaled <- no_tumor * scale_factor
  total_scaled <- tumor_scaled + no_tumor_scaled

  # Create a new dataset with scaled values
  data_scaled <- data.frame(log_dose, tumor = tumor_scaled, no_tumor = no_tumor_scaled, total = total_scaled)

  # Fit logistic regression model
  model <- glm(cbind(tumor, no_tumor) ~ log_dose, family = binomial(link = "logit"), data = data_scaled)

  # Extract variance (square of standard error) for parameters
  variances <- summary(model)$coefficients[, "Std. Error"]^2
  return(variances)
}

main <- function() {
  # Exercise 2:1.1
  # Define the variables for floss usage and periodontitis status
  use <- c("No", "No", "Yes", "Yes") # 'No' and 'Yes' refer to floss usage
  per <- c("No", "Yes", "No", "Yes") # 'No' and 'Yes' refer to periodontitis status
  n <- c(265, 148, 75, 22) # Frequencies for each group
  data21 <- data.frame(use, per, n)

  # Convert categorical variables to factors
  # TODO: is this useless??
  data21$use <- factor(data21$use, levels = c("No", "Yes"))
  data21$per <- factor(data21$per, levels = c("No", "Yes"))

  # Fit the logistic regression model
  # Using the `weights` argument to include the frequencies
  model21 <- glm(per ~ use, weights = n, family = binomial(link = logit), data = data21)

  # Exercise 2:1.2
  # Create a data frame with these variables
  data21b <- data.frame(per, use, n)

  # Convert categorical variables to factors
  data21b$per <- factor(data21b$per, levels = c("No", "Yes"))
  data21b$use <- factor(data21b$use, levels = c("No", "Yes"))
  model21b <- glm(use ~ per, weights = n, family = binomial(link = logit), data = data21b)

  # Exercise 2:2.1
  # Data setup
  log_dose <- c(-7.60, -6.22, -4.60, -3.00, -1.39, 0.92)
  tumor <- c(1, 2, 4, 9, 12, 32)
  no_tumor <- c(17, 17, 24, 23, 16, 8)
  total <- tumor + no_tumor

  # Calculate risk (probability) for each dose
  risk <- tumor / total

  # Calculate log-odds for each dose
  log_odds <- log(risk / (1 - risk))

  # Plot risk against log(dose)
  # plot_risk(log_dose, risk)

  # Plot log-odds against log(dose)
  # plot_odds(log_dose, log_odds)

  # Exercise 2:2.2
  # Create data frame
  data22 <- data.frame(log_dose, tumor, no_tumor, total)
  # Fit logistic regression model
  model22 <- glm(cbind(tumor, no_tumor) ~ log_dose, family = binomial(link = "logit"), data = data22)
  # Summary of the model
  summary(model22)

  # Exercise 2:2.3
  # Step 1: Covariance matrix for the parameter estimates
  cov_matrix <- vcov(model22)

  # Step 2: Correlation assessment
  # Extract standard deviations from diagonal of covariance matrix
  std_errors <- sqrt(diag(cov_matrix))
  correlation <- cov_matrix[1, 2] / (std_errors[1] * std_errors[2])
  correlation <- as.numeric(correlation)

  # Step 3: 95% Confidence intervals for the parameters
  conf_intervals <- confint.default(model22, level = 0.95)

  # Step 4: 95% Confidence interval for the tumor risk at log(dose) = -1.39
  # Calculate the predicted probability for log(dose) = -1.39
  log_dose_value <- -1.39
  predicted_logit <- predict(model22, newdata = data.frame(log_dose = log_dose_value), se.fit = TRUE)
  predicted_probability <- exp(predicted_logit$fit) / (1 + exp(predicted_logit$fit))

  # Calculate the 95% confidence interval for the predicted log-odds
  logit_ci <- c(predicted_logit$fit - 1.96 * predicted_logit$se.fit, predicted_logit$fit + 1.96 * predicted_logit$se.fit)
  # Convert log-odds interval to probability interval
  probability_ci <- exp(logit_ci) / (1 + exp(logit_ci))

  # Exercise 2:2.4
  # Extract the slope coefficient and its standard error
  beta1 <- coef(summary(model22))["log_dose", "Estimate"]
  se_beta1 <- coef(summary(model22))["log_dose", "Std. Error"]

  # Calculate the Wald statistic
  wald_statistic <- (beta1 / se_beta1)^2
  wald_statistic

  # Step 4: Calculate p-value
  p_value24 <- 1 - pchisq(wald_statistic, df = 1)

  # Exercise 2:2.5
  # Calculate variances for different scaling factors
  variance_10 <- scale_and_fit(10, log_dose, tumor, no_tumor)
  variance_100 <- scale_and_fit(100, log_dose, tumor, no_tumor)
  variance_1000 <- scale_and_fit(1000, log_dose, tumor, no_tumor)

  # Combine results into a data frame for comparison
  variance_results <- data.frame(
    Scale_Factor = c(10, 100, 1000),
    Intercept_Variance = c(variance_10[1], variance_100[1], variance_1000[1]),
    Slope_Variance = c(variance_10[2], variance_100[2], variance_1000[2])
  )

  print(variance_results)

  return(list(
    model21 = model21,
    model21b = model21b,
    log_dose = log_dose,
    log_odds = log_odds,
    risk = risk,
    model22 = model22,
    cov_matrix = cov_matrix,
    correlation = correlation,
    conf_intervals = conf_intervals,
    probability_ci = probability_ci,
    beta1 = beta1,
    se_beta1 = se_beta1,
    wald_statistic = wald_statistic,
    p_value24 = p_value24,
    variance_results = variance_results
  ))
}

ass <- main()
