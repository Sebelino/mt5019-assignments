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
  chi2 <- chisq.test(tab1, correct = FALSE)
  library(MASS)
  square_statistics <- loglm(~ gender + opinion, tab1)
  cat("Chi2 and LR statistics:\n")
  print(chi2)

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
  print(summary(square_statistics2))
  odds_ratio2 <- oddsratio(tab2, method = "wald", rev = "both")$measure
  cat("Odds ratio:\n")
  print(odds_ratio2)
  risk_ratio2 <- riskratio(tab2, rev = "both")$measure
  cat("Risk ratio:\n")
  print(risk_ratio2)
  
  return(list(
    percentages = percentages,
    square_statistics = square_statistics,
    odds_ratio = odds_ratio,
    risk_ratio = risk_ratio,
    square_statistics2 = square_statistics2,
    odds_ratio2 = odds_ratio2,
    risk_ratio2 = risk_ratio2
  ))
}

ass1a <- main()