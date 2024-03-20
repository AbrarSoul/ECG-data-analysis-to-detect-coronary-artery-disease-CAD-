# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load dataset from Excel file
data <- read_excel("Projectdata.xls")

# 2.1 Test the statistical difference between patients with CAD and without CAD with respect to distributions of ST depression, ST/HR index, ΔRWA, and maxHR.

# Subset the data for patients with CAD and without CAD
cad_group <- filter(data, CAD == 1)
no_cad_group <- filter(data, CAD == 0)

# Perform t-tests for each variable
t_test_results <- sapply(c("ST-DEP", "ST/HR", "RWA", "MaxHR"), function(var) {
  t.test(cad_group[[var]], no_cad_group[[var]])
})


# 2.2 Test the statistical difference between patients with CAD and without CAD with respect to a) age and b) sex distributions.

# a) Test for age distributions
age_diff <- t.test(data$Age ~ data$CAD)

# b) Test for sex distributions
sex_diff <- chisq.test(table(data$Sex, data$CAD))

# c) What is the probability that you will be wrong if you conclude that Finnish exercise tested patients with CAD are older than those without CAD?
# Let's calculate the probability of Type I error
p_value_age_diff <- age_diff$p.value
alpha <- 0.05
prob_type_1_error <- p_value_age_diff / alpha



# 2.3 a) Are the age distributions of women and men having CAD statistically different?
# Subset the data for women and men with CAD
women_cad <- filter(data, CAD == 1, Sex == "F")
men_cad <- filter(data, CAD == 1, Sex == "M")

# Perform t-test for age distributions
age_diff_gender <- t.test(women_cad$Age, men_cad$Age)

# b) What about distributions of maxHR in women and men having CAD?
# Perform t-test for maxHR distributions
maxHR_diff_gender <- t.test(women_cad$MaxHR, men_cad$MaxHR)

# Display results
print("2.1 Test Results:")
print(t_test_results)
print("2.2 Test Results:")
print(age_diff)
print(sex_diff)
cat("Probability of Type I error for age difference:", prob_type_1_error, "\n")
print("2.3 Test Results:")
print(age_diff_gender)
print(maxHR_diff_gender)
