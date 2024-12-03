model_stats <- function(formula) {
  model <- glm(formula, family = poisson, data = data)
  return(list(
    Deviance = summary(model)$deviance,
    df = summary(model)$df,
    AIC = AIC(model)
  ))
}

main <- function() {
  # Exercise 3:1.1, 3:1.2
  data <- read.csv("data_ca3.csv")

  msat <- glm(n ~ x * y * z * v, family = poisson, data = data)
  cat("Running step from (x+y+z+v)^4 -> (x+y+z+v)^3\n")
  m3 <- step(msat, scope = list(upper = n ~ (x + y + z + v)^4, lower = n ~ (x + y + z + v)^3), direction = "backward", trace = TRUE,)
  # TODO here, assert that p-value of the selected model > 0.05
  cat("Running step from (x+y+z+v)^3 -> (x+y+z+v)^2\n")
  m2 <- step(m3, scope = list(upper = n ~ (x + y + z + v)^3, lower = n ~ (x + y + z + v)^2), direction = "backward", trace = TRUE)
  # TODO here, assert that p-value of the selected model > 0.05
  cat("Running step from (x+y+z+v)^2 -> x+y+z+v\n")
  m1 <- step(m2, scope = list(upper = n ~ (x + y + z + v)^2, lower = n ~ x + y + z + v), direction = "backward", trace = TRUE)
  # TODO here, assert that p-value of the selected model > 0.05

  # Manually entered from the output of running `step` repeatedly
  loglinear_formulas <- list(
    # Four-way formulas
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:z + x:y:v + x:z:v + y:z:v + x:y:z:v",
    # Three-way formulas
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:z + x:y:v + x:z:v + y:z:v",
    # 13-dimensional candidate set
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:v + x:z:v + y:z:v", # - x:y:z, lowest AIC
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:z + x:y:v + x:z:v",
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:z + x:z:v + y:z:v",
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:z + x:y:v + y:z:v",
    # 12-dimensional candidate set
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:v + x:z:v", # - y:z:v, lowest AIC
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:v + y:z:v",
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:z:v + y:z:v",
    # 11-dimensional candidate set
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:y:v", # - x:z:v, lowest AIC
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v + x:z:v",
    # Two-way formulas
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v + z:v",
    # 9-dimensional candidate set
    "n ~ x + y + z + v + x:y + x:z + x:v + y:v + z:v", # - y:z, lowest AIC (best model)
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + z:v",
    "n ~ x + y + z + v + x:y + y:z + x:v + y:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + y:z + y:v + z:v",
    "n ~ x + y + z + v + x:z + y:z + x:v + y:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v",
    # 8-dimensional candidate set (all these suck)
    "n ~ x + y + z + v + x:y + x:v + y:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + x:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + y:v + z:v",
    "n ~ x + y + z + v + x:z + x:v + y:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + x:v + y:v"
  )
  loglinear_models <- list()
  for (formula in loglinear_formulas) {
    loglinear_models[[formula]] <- glm(formula, family = poisson, data = data)
  }

  model_table <- data.frame(
    Model = character(), Deviance = numeric(), df = numeric(),
    p_value = numeric(), AIC = numeric()
  )

  for (model in loglinear_models) {
    is_saturated_model = length(coef(model)) == length(coef(msat)) && all(coef(model) == coef(msat))
    anova_res <- anova(model, msat, test = "LRT")
    p_value <- anova_res$`Pr(>Chi)`[2]
    model_table <- rbind(model_table, data.frame(
      Model = model$formula,
      AIC = AIC(model),
      Deviance = anova_res$Deviance[2],
      df = anova_res$Df[2],
      p_value = ifelse(is_saturated_model, NA, p_value)
    ))
  }
  model_table$Model <- format(model_table$Model, justify="left")
  model_table$p_value <- ifelse(
    model_table$p_value < 0.001,
    format(model_table$p_value, scientific=TRUE, digits=3),
    format(round(model_table$p_value,3), scientific=FALSE)
  )

  # Print results
  print(model_table)

  # Exercise 3:1.3
  best_model_row <- model_table[which.min(model_table$AIC),]
  best_model_formula <- trimws(best_model_row$Model)
  best_model <- loglinear_models[[best_model_formula]]

  # Extract coefficients and standard errors
  coefficients <- summary(best_model)$coefficients
  best_model_estimates <- coefficients[, "Estimate"]
  best_model_se <- coefficients[, "Std. Error"]

  # Compute odds ratios and confidence intervals
  odds_ratios <- exp(best_model_estimates)
  lower_ci <- exp(best_model_estimates - 1.96 * best_model_se)
  upper_ci <- exp(best_model_estimates + 1.96 * best_model_se)

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
