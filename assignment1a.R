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
  # Exercise 1.1.1
  percentages <- percentages(tab1)
  cat("Percentage in favor and against legal abortion:\n")
  print(percentages)

  # Exercise 1.1.2
  chi2 <- chisq.test(tab1, correct = FALSE)
  library(MASS)
  square_statistics <- loglm(~gender+opinion,tab1)

  return(list(
    percentages = percentages,
    square_statistics = square_statistics
  ))
}

ass1a <- main()
