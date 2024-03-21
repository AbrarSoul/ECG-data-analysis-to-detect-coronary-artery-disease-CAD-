# Load required libraries
library(readxl)
library(ggplot2)
library(car)

# Load the dataset
data <- read_excel("Projectdata.xls")

# Check the structure of the dataset
str(data)

# Summary statistics
summary(data)

# 3.1 Correlation and regression between two variables

# a) Patients with CAD
cad_data <- subset(data, CAD == 1)
cor_cad <- cor.test(cad_data$`ST-DEP`, cad_data$`ST/HR`)
summary(lm(`RWA` ~ `ST-DEP` + `ST/HR`, data = cad_data))

# b) Patients without CAD
no_cad_data <- subset(data, CAD == 0)
cor_no_cad <- cor.test(no_cad_data$`ST-DEP`, no_cad_data$`ST/HR`)
summary(lm(`RWA` ~ `ST-DEP` + `ST/HR`, data = no_cad_data))


# 3.2 Linear regression between maxHR and age

# a) Patients without CAD
lm_no_cad <- lm(MaxHR ~ Age, data = no_cad_data)
summary(lm_no_cad)
plot(no_cad_data$Age, no_cad_data$MaxHR, main = "MaxHR vs Age (No CAD)",
     xlab = "Age", ylab = "MaxHR")
abline(lm_no_cad, col = "red")

# b) Patients with CAD
lm_cad <- lm(MaxHR ~ Age, data = cad_data)
summary(lm_cad)
plot(cad_data$Age, cad_data$MaxHR, main = "MaxHR vs Age (CAD)",
     xlab = "Age", ylab = "MaxHR")
abline(lm_cad, col = "blue")


# 3.3 Regression analysis between maxHR and ST-DEP/ST/HR index

lm_maxHR_ST_DEP <- lm(MaxHR ~ `ST-DEP`, data = data)
summary(lm_maxHR_ST_DEP)

lm_maxHR_ST_HR <- lm(MaxHR ~ `ST/HR`, data = data)
summary(lm_maxHR_ST_HR)


# 3.4 Relationship between age and RWA in total population
cor_total <- cor.test(data$Age, data$`RWA`)
summary(cor_total)
