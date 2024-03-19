# Load necessary libraries
library(psych)  # For descriptive statistics
library(plyr)
library(ggplot2)
library(ggpubr)
library(car)

# Load dataset from Excel file
data <- read_excel("Projectdata.xls")

# Display the structure of the dataset
str(data)

# 1.1 Descriptive statistics

# Calculate mean, standard deviation, and 95% confidence interval for specified variables
desc_stats <- describe(data[c("Age", "MaxHR", "ST-DEP", "ST/HR", "RWA")])

# Function to calculate confidence interval
ci <- function(x) {c(mean(x) - 1.96 * sd(x)/sqrt(length(x)), mean(x) + 1.96 * sd(x)/sqrt(length(x)))}

# Calculate confidence intervals for specified variables by CAD status
ci_age <- ci(data$Age)
ci_maxhr <- ci(data$MaxHR)
ci_st_dep <- ci(data$ST_DEP)
ci_st_hr <- ci(data$ST_HR)
ci_rwa <- ci(data$RWA)

# Create a table for descriptive statistics
descriptive_table <- data.frame(
  Variable = c("Age", "MaxHR", "ST-DEP", "ST/HR", "RWA"),
  Mean = desc_stats$mean,
  SD = desc_stats$sd,
  CI_Lower = c(ci_age[1], ci_maxhr[1], ci_st_dep[1], ci_st_hr[1], ci_rwa[1]),
  CI_Upper = c(ci_age[2], ci_maxhr[2], ci_st_dep[2], ci_st_hr[2], ci_rwa[2])
)

# Sex distribution
sex_distribution <- table(data$Sex)

# Number of cases with infarcts and medications in both groups
cases_infarcts <- table(data$CAD, data$MI)
cases_medications <- table(data$CAD, data$Beta, data$Calsi, data$Digit, data$Nitro)

# Print descriptive statistics table
print("Descriptive Statistics:")
print(descriptive_table)

# Print sex distribution
print("Sex Distribution:")
print(sex_distribution)

# Print number of cases with infarcts
print("Number of Cases with Infarcts:")
print(cases_infarcts)

# Print number of cases with medications
print("Number of Cases with Medications:")
print(cases_medications)

# 1.2 Test for normality using Shapiro-Wilk test

# Function to perform Shapiro-Wilk test and print results
test_normality <- function(x, group) {
  shapiro_test <- shapiro.test(x)
  cat("Shapiro-Wilk Test for Normality (", group, "):\n")
  print(shapiro_test)
  if (shapiro_test$p.value < 0.05) {
    cat("The distribution is not normal.\n")
  } else {
    cat("The distribution is normal.\n")
  }
}

# Test normality for MaxHR in patients without CAD
test_normality(data$MaxHR[data$CAD == 0], "Patients without CAD")

# Test normality for MaxHR in patients with CAD
test_normality(data$MaxHR[data$CAD == 1], "Patients with CAD")