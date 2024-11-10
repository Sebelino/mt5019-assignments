percentages <- function() {
  # Exercise 1.1.1
  cat("Percentage in favor and against legal abortion:\n")
  tab1 <- as.table(rbind(c(309, 191), c(319, 281)))
  dimnames(tab1) <- list(gender = c("Women", "Men"), opinion = c("In favor", "Against"))
  addmargins(prop.table(tab1, 1), 2)
  percentages <- prop.table(tab1, 1) * 100
  percentages <- apply(percentages, c(1, 2), function(x) sprintf("%.2f%%", x))
  percentages <- noquote(percentages)
  print(percentages)
}

ass1a <- list(
  percentages = percentages()
)
