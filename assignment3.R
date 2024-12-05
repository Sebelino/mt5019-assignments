main <- function() {
  # Exercise 3:1.1, 3:1.2
  data <- read.csv("data_ca3.csv")

  msat <- glm(n ~ x * y * z * v, family = poisson, data = data)

  cat("Running step from (x+y+z+v)^4 -> (x+y+z+v)^3\n")
  m3 <- step(msat, scope = list(upper = n ~ (x + y + z + v)^4, lower = n ~ (x + y + z + v)^3), direction = "backward", trace = TRUE)
  cat("Running step from (x+y+z+v)^3 -> (x+y+z+v)^2\n")
  m2 <- step(m3, scope = list(upper = n ~ (x + y + z + v)^3, lower = n ~ (x + y + z + v)^2), direction = "backward", trace = TRUE)
  cat("Running step from (x+y+z+v)^2 -> x+y+z+v\n")
  m1 <- step(m2, scope = list(upper = n ~ (x + y + z + v)^2, lower = n ~ x + y + z + v), direction = "backward", trace = TRUE)

  # Manually entered from the output of running `step` repeatedly
  loglinear_formulas <- c(
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
    "n ~ x + y + z + v + x:z + y:z + x:v + y:v + z:v", # p<0.05
    "n ~ x + y + z + v + x:y + x:z + y:z + x:v + y:v", # p<0.05
    # 8-dimensional candidate set (all these suck)
    "n ~ x + y + z + v + x:y + x:v + y:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + x:v + z:v",
    "n ~ x + y + z + v + x:y + x:z + y:v + z:v",
    "n ~ x + y + z + v + x:z + x:v + y:v + z:v", # p<0.05
    "n ~ x + y + z + v + x:y + x:z + x:v + y:v" # p<0.05
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
    is_saturated_model <- length(coef(model)) == length(coef(msat)) && all(coef(model) == coef(msat))
    anova_res <- anova(model, msat, test = "LRT")
    p_value <- anova_res$`Pr(>Chi)`[2]
    model_table <- rbind(model_table, data.frame(
      Model = compress_formula(model$formula),
      Formula = model$formula,
      AIC = AIC(model),
      Deviance = anova_res$Deviance[2],
      df = anova_res$Df[2],
      p_value = ifelse(is_saturated_model, NA, p_value)
    ))
  }
  model_table$Formula <- format(model_table$Formula, justify = "left")
  model_table$p_value <- fmt_decimal(model_table, "p_value")

  model_table_short <- model_table[, !(colnames(model_table) %in% "Formula")]

  # Print results
  print(model_table)

  # Exercise 3:1.3
  # Here we should technically assert that p-value of the selected model > 0.05
  best_model_row <- model_table[which.min(model_table$AIC), ]
  best_model_formula <- trimws(best_model_row$Formula)
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
  associations$P_Value <- fmt_decimal(associations, "P_Value")

  # Print results
  print(associations)

  # Exercise 3:1.4
  # Fit the model with main effects and interactions
  msat4 <- glm(v ~ x * y * z, weights = n, family = binomial, data = data)
  model4 <- step(msat4, direction = "backward", trace = TRUE, scope = list(upper = v ~ x * y * z, lower = v ~ x + y + z))

  # Deviance relative to saturated model
  anova_msat4 <- anova(model4, msat4, test = "LRT")

  # Deviance relative to the second simplest model
  second_simplest_model <- glm(v ~ x + y + z + x:z, weights = n, family = binomial, data = data)
  anova_m <- anova(model4, second_simplest_model, test = "LRT")
  
  # Extract coefficients
  coefficients4 <- summary(model4)$coefficients
  beta4 <- coefficients4[, "Estimate"]
  se4 <- coefficients4[, "Std. Error"]
  
  # Compute odds ratios and confidence intervals
  odds_ratios4 <- exp(beta4)
  lower_ci4 <- exp(beta4 - 1.96 * se4)
  upper_ci4 <- exp(beta4 + 1.96 * se4)
  
  # Combine results into a data frame
  results4 <- data.frame(
    Odds_Ratio = odds_ratios4,
    Lower_CI = lower_ci4,
    Upper_CI = upper_ci4,
    P_Value = coefficients4[, "Pr(>|z|)"]
  )
  results4$P_Value <- fmt_decimal(results4, "P_Value")
  print(results4)

  # Exercise 3:1.5
  # https://stats.stackexchange.com/questions/476742/cant-find-loglinear-models-corresponding-logistic-regression-model
  # https://teaching.sociology.ul.ie/SSS/lugano/node58.html
  model4_loglinear <- glm(
    n ~ x + y + z + v
    + x:y
    + x:z
    + x:v
    + y:z
    + y:v
    + z:v
    + x:y:z,
    family = poisson(link = log), data = data
  )
  coefficients4_ll <- summary(model4_loglinear)$coefficients
  beta4_ll <- coefficients4_ll[, "Estimate"]
  se4_ll <- coefficients4_ll[, "Std. Error"]

  comparison <- data.frame(
    LogisticParam = c("(Intercept)", "x", "y", "z"),
    LoglinearParam = c("v", "x:v", "y:v", "z:v"),
    Logistic = beta4[c("(Intercept)", "x", "y", "z")],
    Loglinear = beta4_ll[c("v", "x:v", "y:v", "z:v")],
    Difference = c(
      beta4_ll["v"] - beta4["(Intercept)"],
      beta4_ll["x:v"] - beta4["x"],
      beta4_ll["y:v"] - beta4["y"],
      beta4_ll["z:v"] - beta4["z"]
    ),
    row.names = NULL
  )
  print(comparison)
  
  comparison_se <- data.frame(
    LogisticParam = c("(Intercept)", "x", "y", "z"),
    LoglinearParam = c("v", "x:v", "y:v", "z:v"),
    Logistic = se4[c("(Intercept)", "x", "y", "z")],
    Loglinear = se4_ll[c("v", "x:v", "y:v", "z:v")],
    Difference = c(
      se4_ll["v"] - se4["(Intercept)"],
      se4_ll["x:v"] - se4["x"]
    ),
    row.names = NULL
  )
  print(comparison_se)
  
  return(list(
    data = data,
    msat = msat,
    model_table = model_table,
    model_table_short = model_table_short,
    best_model = best_model,
    associations = associations,
    msat4 = msat4,
    model4 = model4,
    anova_msat4 = anova_msat4,
    anova_m = anova_m,
    results4 = results4,
    beta4 = beta4,
    se4 = se4,
    beta4_ll = beta4_ll,
    se4_ll = se4_ll,
    comparison = comparison,
    comparison_se = comparison_se
  ))
}

fmt_decimal <- function(tbl, colname) {
  ifelse(
    tbl[[colname]] < 0.001,
    format(tbl[[colname]], scientific = TRUE, digits = 3),
    format(round(tbl[[colname]], 3), scientific = FALSE)
  )
}

extract_terms <- function(term_list, pattern) {
  terms <- sapply(term_list[grep(pattern, term_list)], function(term) gsub(":", "", term))
  return(terms)
}

is_included <- function(term, term_list) {
  return(any(sapply(term_list, function(string) all(strsplit(term, NULL)[[1]] %in% strsplit(string, NULL)[[1]]))))
}

compress_formula <- function(f) {
  term_string <- trimws(unlist(strsplit(f, "~"))[[2]])
  term_list <- trimws(unlist(strsplit(term_string, "\\+")))
  orders <- sapply(term_list, function(term) {
    m <- gregexpr(":", term)[[1]]
    return(1 + ifelse(m == -1, 0, length(m)))
  })
  fourth_order_terms <- extract_terms(term_list, "^.:.:.:.$")
  third_order_terms <- extract_terms(term_list, "^.:.:.$")
  second_order_terms <- extract_terms(term_list, "^.:.$")
  first_order_terms <- extract_terms(term_list, "^.$")
  compressed_term_list <- character()
  for (term in fourth_order_terms) {
    compressed_term_list <- append(compressed_term_list, term)
  }
  for (term in third_order_terms) {
    if (!is_included(term, compressed_term_list)) {
      compressed_term_list <- append(compressed_term_list, term)
    }
  }
  for (term in second_order_terms) {
    if (!is_included(term, compressed_term_list)) {
      compressed_term_list <- append(compressed_term_list, term)
    }
  }
  for (term in first_order_terms) {
    if (!is_included(term, compressed_term_list)) {
      compressed_term_list <- c(compressed_term_list, term)
    }
  }

  model_name <- toupper(paste(compressed_term_list, collapse = ","))

  result <- list(
    orders = orders,
    first_order_terms = first_order_terms,
    second_order_terms = second_order_terms,
    third_order_terms = third_order_terms,
    fourth_order_terms = fourth_order_terms,
    compressed_term_list = compressed_term_list,
    model_name = model_name
  )
  result <- result$model_name
  return(result)
}

ass <- main()
