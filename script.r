#!/usr/bin/Rscript

# (c) 2024 Ondřej Hasník - MIT License

UKA <- c(2, 2.5, 3.7, 4, 3.6, 3, 2.7, 2.7, 3.4, 3)
Control <- c(4, 4.5, 3.7, 3.6, 3.2, 4.7, 2.8, 3.8, 4.1, 4.5)

result <- t.test(UKA, Control, alternative = "two.sided", paired = FALSE)

sd_UKA <- sd(UKA)
sd_Control <- sd(Control)

var_UKA <- var(UKA)
var_Control <- var(Control)

alpha <- 0.05
df <- result$parameter
critical_value <- qt(alpha/2, df)

data <- data.frame(
  Statistic = c("Mean", "Standard deviation", "Variance", "Count (n)", "t-value", "Degrees of freedom", "Critical value", "p-value", "Significance level"),
  UKA = c(mean(UKA), sd_UKA, var_UKA, length(UKA), result$statistic, df, critical_value, result$p.value, alpha),
  Control = c(mean(Control), sd_Control, var_Control, length(Control), "", "", "", "", "")
)

print(data)
