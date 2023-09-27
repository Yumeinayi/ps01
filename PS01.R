#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessay packages
library(stargazer)
library(ggplot2)

lapply(c(" "),  pkgTest)



#####################
# Problem 1
#####################

# Given data
IQ_scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Perform a t-test
t_test_result <- t.test(IQ_scores, mu = 100, alternative = "greater", conf.level = 0.90)

# Print t-test results
cat("1. 90% Confidence Interval for School Students' Average IQ:\n")
print(t_test_result$conf.int)

cat("\n2. Hypothesis Test Results:\n")
cat("   Test Statistic (t):", t_test_result$statistic, "\n")
cat("   Degrees of Freedom:", t_test_result$parameter, "\n")
cat("   p-value:", t_test_result$p.value, "\n")

# Determine whether to reject the null hypothesis based on the p-value
if (t_test_result$p.value < 0.05) {
  cat("   At a significance level of 0.05, we reject the null hypothesis. There is enough evidence to conclude that school students' average IQ is higher than the national average.\n")
} else {
  cat("   At a significance level of 0.05, we fail to reject the null hypothesis. There is not enough evidence to conclude that school students' average IQ is higher than the national average.\n")
}

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

# Load the data
data <- expenditure

# Scatterplot matrix
pairs(data[c("Y", "X1", "X2", "X3")], main="Scatterplot Matrix")

# Calculate correlations
correlations <- cor(data[c("Y", "X1", "X2", "X3")])
print(correlations)

# Create a boxplot
ggplot(data, aes(x = Region, y = Y)) +
  geom_boxplot() +
  labs(x = "Region", y = "Per Capita Expenditure") +
  theme_minimal()

# Create a scatterplot with different symbols and colors for regions
ggplot(data, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point() +
  labs(x = "Per Capita Income", y = "Per Capita Expenditure") +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")) +
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18, "4" = 19)) +
  theme_minimal()