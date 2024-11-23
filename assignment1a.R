make_table <- function() {
  tab1 <- as.table(rbind(c(309, 191), c(319, 281)))
  dimnames(tab1) <- list(gender = c("Women", "Men"), opinion = c("In favor", "Against"))
  return(tab1)
}

percentages <- function(tab1) {
  addmargins(prop.table(tab1, 1), 2)
  percentages <- prop.table(tab1, 1) * 100
  percentages <- apply(percentages, c(1, 2), function(x) sprintf("%.2f%%", x))
  percentages <- noquote(percentages)
  return(percentages)
}

main <- function() {
  tab1 <- make_table()
  # Exercise 1:1.1
  percentages <- percentages(tab1)
  cat("Percentage in favor and against legal abortion:\n")
  print(percentages)

  # Exercise 1:1.2
  library(MASS)
  square_statistics <- loglm(~ gender + opinion, tab1)
  cat("Chi2 and LR statistics:\n")
  print(square_statistics)

  # Exercise 1:1.3
  library(epitools)
  odds_ratio <- oddsratio(tab1, method = "wald", rev = "both")$measure
  cat("Odds ratio:\n")
  print(odds_ratio)

  # Exercise 1:1.4
  risk_ratio <- riskratio(tab1, rev = "both")$measure
  cat("Risk ratio:\n")
  print(risk_ratio)
  
  # Exercise 1:2.1
  tab2 <- as.table(rbind(c(1198, 1493), c(557, 1278)))
  dimnames(tab2) <- list(gender = c("men", "women"), admission = c("admitted", "not_admitted"))
  square_statistics2 <- loglm(~ gender + admission, data = tab2)
  cat("Chi2 and LR statistics:\n")
  print(square_statistics2)
  odds_ratio2 <- oddsratio(tab2, method = "wald", rev = "both")$measure
  cat("Odds ratio:\n")
  print(odds_ratio2)
  risk_ratio2 <- riskratio(tab2, rev = "both")$measure
  cat("Risk ratio:\n")
  print(risk_ratio2)
  
  # Exercise 1:2.2
  # Divide study population by 10
  tab2b <- round(tab2/10, 0)
  cat("Dividing the study population by 10:\n")
  cat("Table:")
  print(tab2b)
  square_statistics2b <- loglm(~ gender + admission, data = tab2b)
  cat("Chi2 and LR statistics:\n")
  print(square_statistics2b)
  odds_ratio2b <- oddsratio(tab2b, method = "wald", rev = "both")$measure
  cat("Odds ratio:\n")
  print(odds_ratio2b)
  risk_ratio2b <- riskratio(tab2b, rev = "both")$measure
  cat("Risk ratio:\n")
  print(risk_ratio2b)
  # Divide study population by 100
  cat("Dividing the study population by 100:\n")
  tab2c <- round(tab2/100, 0)
  cat("Table:")
  print(tab2c)
  square_statistics2c <- loglm(~ gender + admission, data = tab2c)
  cat("Chi2 and LR statistics:\n")
  print(square_statistics2c)
  odds_ratio2c <- oddsratio(tab2c, method = "wald", rev = "both")$measure
  cat("Odds ratio:\n")
  print(odds_ratio2c)
  risk_ratio2c <- riskratio(tab2c, rev = "both")$measure
  cat("Risk ratio:\n")
  print(risk_ratio2c)
  
  # Exerices 1:2.3
  cat("Custom study:\n")
  tab2d <- matrix(c(500000, 501500, 500500, 499000), nrow = 2, byrow = TRUE)
  cat("Table:")
  print(tab2d)
  dimnames(tab2d) <- list(gender = c("men", "women"), admission = c("admitted", "not_admitted"))
  square_statistics2d <- loglm(~ gender + admission, data = tab2d)
  cat("Chi2 and LR statistics:\n")
  print(square_statistics2d)
  cat("Odds ratio:\n")
  odds_ratio2d <- oddsratio(tab2d, method = "wald", rev = "both")$measure
  print(odds_ratio2d)
  
  return(list(
    percentages = percentages,
    square_statistics = square_statistics,
    odds_ratio = odds_ratio,
    risk_ratio = risk_ratio,
    tab2 = tab2,
    square_statistics2 = square_statistics2,
    odds_ratio2 = odds_ratio2,
    risk_ratio2 = risk_ratio2,
    tab2b = tab2b,
    square_statistics2b = square_statistics2b,
    odds_ratio2b = odds_ratio2b,
    risk_ratio2b = risk_ratio2b,
    tab2c = tab2c,
    square_statistics2c = square_statistics2c,
    odds_ratio2c = odds_ratio2c,
    risk_ratio2c = risk_ratio2c,
    tab2d = tab2d,
    square_statistics2d = square_statistics2d,
    odds_ratio2d = odds_ratio2d
  ))
}

ass1a <- main()