main <- function() {
  # Exercise 3:1.1, 3:1.2
  # Load the data
  data <- read.csv("data_ca3.csv")

  saturated_model <- glm(n ~ x * y * z * v, family = poisson, data = data)

  # xyzv, xyv, xzv, yzv, xyz, xv, yv, zv, xy, xz, yz, x, y, z, v
  models <- list(
    "xyzv (Saturated)" = saturated_model,
    "xyv, xzv, yzv, xyz" = glm(n ~ x * y * v + x * z * v + y * z * v + x * y * z, family = poisson, data = data),
    "xzv, yzv, xyz" = glm(n ~ x * z * v + y * z * v + x * y * z, family = poisson, data = data),
    "yzv, xyz, xv" = glm(n ~ y * z * v + x * y * z + x * v, family = poisson, data = data),
    "xyz, xv, yv, zv" = glm(n ~ x * y * z + x * v + y * v + z * v, family = poisson, data = data),
    "xv, yv, zv, xz, xy, yz" = glm(n ~ x * v + y * v + z * v + x * z + x * y + y * z, family = poisson, data = data),

    # "xv, zv, xy" = glm(n ~ x*v+z*v+x*y, family = poisson, data = data),
    "xy, xz, xv, yv, zv" = glm(n ~ x * y + x * z + x * v + y * v + z * v, family = poisson, data = data),
    #
    "yv, zv, xz, xy, yz" = glm(n ~ y * v + z * v + x * z + x * y + y * z, family = poisson, data = data),
    "zv, xz, xy, yz" = glm(n ~ z * v + x * z + x * y + y * z, family = poisson, data = data),
    "xz, xy, yz, v" = glm(n ~ x * z + x * y + y * z + v, family = poisson, data = data),
    "xy, yz, v" = glm(n ~ x * y + y * z + v, family = poisson, data = data),
    "yz, v, x" = glm(n ~ y * z + v + x, family = poisson, data = data),
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
  best_model <- models$"xv, yv, zv, xz, xy, yz"

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

  data4 <- data

  saturated_model4 <- glm(v ~ x * y * z, weights = n, data = data4, family = binomial)

  models4 <- list(
    "xyz (Saturated)" = saturated_model4,
    "xy, xz, yz" = glm(v ~ x * y + x * z + y * z, family = binomial, weights = n, data = data4),
    "xy, xz" = glm(v ~ x * y + x * z, family = binomial, weights = n, data = data4),
    "xy, yz" = glm(v ~ x * y + y * z, family = binomial, weights = n, data = data4),
    "yz, xz" = glm(v ~ y * z + x * z, family = binomial, weights = n, data = data4),
    "yz, x" = glm(v ~ y * z + x, family = binomial, weights = n, data = data4),
    "xz, y" = glm(v ~ x * z + y, family = binomial, weights = n, data = data4),
    "xy, z" = glm(v ~ x * y + z, family = binomial, weights = n, data = data4),
    "x, y, z" = glm(v ~ x + y + z, family = binomial, weights = n, data = data4)
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
      Deviance = anova_res$Deviance[2],
      df = anova_res$Df[2],
      p_value = ifelse(model_name == "xyz (Saturated)", NA, anova_res$`Pr(>Chi)`[2]),
      AIC = AIC(model)
    ))
  }

  print(summary(saturated_model4))

  model4 <- models4$"x, y, z"

  # Extract coefficients
  coefficients <- summary(model4)$coefficients
  beta <- coefficients[, "Estimate"]
  se <- coefficients[, "Std. Error"]

  # Compute odds ratios and confidence intervals
  odds_ratios <- exp(beta)
  lower_ci <- exp(beta - 1.96 * se)
  upper_ci <- exp(beta + 1.96 * se)

  # Combine results into a data frame
  results4 <- data.frame(
    Odds_Ratio = odds_ratios,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    P_Value = coefficients[, "Pr(>|z|)"]
  )

  # Print results
  print(results4)

  # Exercise 3:1.5

  print(data)
  model4_loglinear <- glm(n ~ x + y + z + v, family = poisson, data = data)

  return(list(
    model_table = model_table,
    best_model = best_model,
    associations = associations,
    model_table4 = model_table4,
    results4 = results4,
    model4 = model4,
    model4_loglinear = model4_loglinear
  ))
}

ass <- main()
