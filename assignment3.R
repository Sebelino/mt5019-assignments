main <- function() {
  # Exercise 3:1.1, 3:1.2
  # Load the data
  data <- read.csv("data_ca3.csv")

  saturated_model <- glm(n ~ x * y * z * v, family = poisson, data = data)

  models <- list(
    "xyzv (Saturated)" = saturated_model,
    "xyz, xyv, xzv, yzv" = glm(n ~ x * y * z + x * y * v + x * z * v + y * z * v, family = poisson, data = data),
    "xyv, xzv, yzv" = glm(n ~ x * y * v + x * z * v + y * z * v, family = poisson, data = data),
    "xyz, xv, yv, zv" = glm(n ~ x * y * z + x * y + y * v + z * v, family = poisson, data = data),
    "yz, yv, zv, x" = glm(n ~ y * z + y * v + z * v + x, family = poisson, data = data),
    "xz, xv, zv, y" = glm(n ~ x * z + x * y + z * v + y, family = poisson, data = data),
    "xy, xv, yv, z" = glm(n ~ x * y + x * y + y * v + z, family = poisson, data = data),
    "xy, xz, yz, v" = glm(n ~ x * y + x * y + y * z + v, family = poisson, data = data),
    "zv, x, y" = glm(n ~ z * v + x + y, family = poisson, data = data),
    "yv, x, z" = glm(n ~ y * v + x + y, family = poisson, data = data),
    "yz, x, v" = glm(n ~ y * z + x + y, family = poisson, data = data),
    "xv, y, z" = glm(n ~ x * v + y + z, family = poisson, data = data),
    "xz, y, v" = glm(n ~ x * z + y + v, family = poisson, data = data),
    "xy, z, v" = glm(n ~ x * y + z + v, family = poisson, data = data),
    "x, y, z, v" = glm(n ~ x + y + z + v, family = poisson, data = data)
  )

  # Compare models
  model_table <- data.frame(
    Model = character(), Deviance = numeric(), df = numeric(),
    p_value = numeric(), AIC = numeric()
  )

  for (model_name in names(models)) {
    model <- models[[model_name]]
    anova_res <- anova(model, saturated_model, test = "LRT")
    model_table <- rbind(model_table, data.frame(
      Model = model_name,
      Deviance = anova_res$Deviance[2],
      df = anova_res$Df[2],
      p_value = ifelse(model_name == "xyzv (Saturated)", NA, anova_res$`Pr(>Chi)`[2]),
      AIC = AIC(model)
    ))
  }

  # Print results
  print(model_table)

  # Exercise 3:1.3
  best_model <- models$"xz, xv, zv, y"

  summary_model_6 <- summary(best_model)

  # Extract coefficients and standard errors
  coefficients <- summary_model_6$coefficients
  beta <- coefficients[, "Estimate"]
  se <- coefficients[, "Std. Error"]

  # Compute odds ratios and confidence intervals
  odds_ratios <- exp(beta)
  lower_ci <- exp(beta - 1.96 * se)
  upper_ci <- exp(beta + 1.96 * se)

  # Combine results into a data frame for interpretation
  associations <- data.frame(
    Odds_Ratio = odds_ratios,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    P_Value = coefficients[, "Pr(>|z|)"]
  )

  # Print results
  print(associations)

  # Exercise 3:1.4
  # Fit the model with main effects and interactions
  saturated_model4 <- glm(v ~ x * y * z, weights = n, data = data, family = binomial)

  models4 <- list(
    "xyz (Saturated)" = saturated_model4,
    "xy, xz, yz" = glm(v ~ x * y + x * z + y * z, family = binomial, weights = n, data = data),
    "yz, x" = glm(v ~ y * z + x, family = binomial, weights = n, data = data),
    "xz, y" = glm(v ~ x * z + y, family = binomial, weights = n, data = data),
    "xy, z" = glm(v ~ x * y + z, family = binomial, weights = n, data = data),
    "x, y, z" = glm(v ~ x + y + z, family = binomial, weights = n, data = data)
  )

  # Compare models
  model_table4 <- data.frame(
    Model = character(), Deviance = numeric(), df = numeric(),
    p_value = numeric(), AIC = numeric()
  )

  for (model_name in names(models4)) {
    model <- models4[[model_name]]
    anova_res <- anova(model, saturated_model4, test = "LRT")
    model_table4 <- rbind(model_table4, data.frame(
      Model = model_name,
      Deviance = ifelse(model_name == "xyz (Saturated)", NA, anova_res$Deviance[2]),
      df = ifelse(model_name == "xyz (Saturated)", NA, anova_res$Df[2]),
      p_value = ifelse(model_name == "DUMMYxyz (Saturated)", NA, anova_res$`Pr(>Chi)`[2]),
      AIC = AIC(model)
    ))
  }

  print(summary(saturated_model4))

  # Extract coefficients
  coefficients <- summary(model)$coefficients
  beta <- coefficients[, "Estimate"]
  se <- coefficients[, "Std. Error"]

  # Compute odds ratios and confidence intervals
  odds_ratios <- exp(beta)
  lower_ci <- exp(beta - 1.96 * se)
  upper_ci <- exp(beta + 1.96 * se)

  # Combine results into a data frame
  results <- data.frame(
    Odds_Ratio = odds_ratios,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    P_Value = coefficients[, "Pr(>|z|)"]
  )

  # Print results
  print(results)

  # Compare models with and without interactions
  model_main <- glm(v ~ x + y + z, data = data, family = binomial(link = "logit"))
  AIC(model_main, model)

  return(list(
    model_table = model_table,
    best_model = best_model,
    associations = associations
  ))
}

ass <- main()
