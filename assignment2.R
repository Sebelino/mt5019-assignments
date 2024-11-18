main <- function() {
  # Exercise 2:1.1
  # Step 1: Set up the data
  # Define the variables for floss usage and periodontitis status
  use <- c("No", "No", "Yes", "Yes") # 'No' and 'Yes' refer to floss usage
  per <- c("No", "Yes", "No", "Yes") # 'No' and 'Yes' refer to periodontitis status
  n <- c(265, 148, 75, 22) # Frequencies for each group

  # Create a data frame with these variables
  data21 <- data.frame(use, per, n)

  # Step 2: Convert categorical variables to factors
  data21$use <- factor(data21$use, levels = c("No", "Yes"))
  data21$per <- factor(data21$per, levels = c("No", "Yes"))

  # Step 3: Fit the logistic regression model
  # Using the `weights` argument to include the frequencies
  model21 <- glm(per ~ use, weights = n, family = binomial(link = "logit"), data = data21)

  # Exercise 2:1.2
  # Create a data frame with these variables
  data21b <- data.frame(per, use, n)

  # Convert categorical variables to factors
  data21b$per <- factor(data21b$per, levels = c("No", "Yes"))
  data21b$use <- factor(data21b$use, levels = c("No", "Yes"))
  model21b <- glm(use ~ per, weights = n, family = binomial(link = "logit"), data = data21b)

  return(list(
    model21 = model21,
    model21b = model21b
  ))
}

ass <- main()
