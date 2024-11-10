# Exercise 1.1.1
cat("Percentage in favor and against legal abortion:\n")
tab1 <- as.table(rbind(c(309,191), c(319,281)))
dimnames(tab1) <- list(gender=c("Women","Men"), opinion=c("In favor", "Against"))
addmargins(prop.table(tab1,1),2)
percentages <- prop.table(tab1, 1) * 100
percentages <- apply(percentages, c(1, 2), function(x) sprintf("%.2f%%", x))
print(percentages)


# 2. Do men and women have different opinions on legal abortion? De�ne a null and an alternative hypothesis and report all steps to calculate Pearsons X2 statistic and the likelihood ratio statistic, G2. What conclusion can be made from this survey.
# 3. Report all steps to estimate an odds ratio together with a 95 % confidence interval. Note that it is possible to formulate several odds ratios for a twoway table, depending on which event the odds refer to and what you conditioning on (an odds ratio is an ratio of two conditional odds). Interpret the estimated odds ratio and the corresponding con�dence interval in the context of the survey.
# 4. Report all steps to estimate a risk ratio (relative risk) together with a 95 % confidence interval. The risk ratio should correspond to the odds ratio you reported in question 3. Interpret the risk ratio and the confidence interval in the same way as in question 3.
# 5. Below is shown how available R-functions can be used to perform the calculations in question 1 to 4 (other R-functions might work as well). Run the code and check that your previous calculations are correct. Regarding the odds ratio and risk ratio, you might need to change the rev option in R (see below) to get agreement between them.
