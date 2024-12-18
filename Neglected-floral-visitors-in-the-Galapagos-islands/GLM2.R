install.packages("dplyr")
install.packages("pscl")
install.packages("Rcpp")
install.packages("car")
install.packages('ggplot2', dep = TRUE)
install.packages('data.table', dep = TRUE)
install.packages("effects")
install.packages("scales")

library(dplyr)
library(pscl)
library(Rcpp)
library(car)
library(ggplot2)
library(data.table)
library(effects)
library(scales)

setwd("~/IMEDEA/DEPICT/Galapagos/DATOS/Ants/Neglected-floral-visitors-in-the-Galapagos-islands")
data <- read.csv("General.csv")
View(data)
head(data)

# Convert relevant columns to factors
data$Organism <- as.factor(data$Organism)
data$Status <- as.factor(data$Status)
data$Island <- as.factor(data$Island)

# Verify the changes
str(data)

#fit glm model Posison #
glm_model <- glm(formula = Interac ~ Status + Organism + Status:Organism, 
    family = poisson(link = "log"), data = data)

# Summarize the model glim
summary(glm_model)

# Fit the zero-inflated Poisson model
zip_model <- zeroinfl(Interac ~ Organism * Status + Island, data = data, dist = "poisson")

# Summarize the model
summary_zip <- summary(zip_model)

# Extract count model coefficients
count_coef <- summary_zip$coefficients$count
count_coef <- data.frame(count_coef)

# Extract zero-inflation model coefficients
zero_inflation_coef <- summary_zip$coefficients$zero
zero_inflation_coef <- data.frame(zero_inflation_coef)

# Display the count model coefficients
print("Count Model Coefficients (Poisson with Log Link)")
print(count_coef)

# Display the zero-inflation model coefficients
print("Zero-Inflation Model Coefficients (Binomial with Logit Link)")
print(zero_inflation_coef)

# Create an ANOVA-like table for the zero-inflated Poisson model
anova_table <- Anova(zip_model, type = "III")

# Display the ANOVA table
print(anova_table)

# Extract coefficients and confidence intervals
coef_data <- data.frame(
  term = rownames(summary(zip_model)$coefficients$count),
  estimate = summary(zip_model)$coefficients$count[, "Estimate"],
  conf.low = summary(zip_model)$coefficients$count[, "Estimate"] - 1.96 * summary(zip_model)$coefficients$count[, "Std. Error"],
  conf.high = summary(zip_model)$coefficients$count[, "Estimate"] + 1.96 * summary(zip_model)$coefficients$count[, "Std. Error"]
)

# Plot coefficients with confidence intervals
ggplot(coef_data, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Coefficient Plot", x = "Terms", y = "Estimates")

# Coefficient Plot
# Extract coefficients and confidence intervals
coef_data <- summary(zip_model)$coefficients$count
coef_df <- data.frame(
  term = rownames(coef_data),
  estimate = coef_data[, "Estimate"],
  conf.low = coef_data[, "Estimate"] - 1.96 * coef_data[, "Std. Error"],
  conf.high = coef_data[, "Estimate"] + 1.96 * coef_data[, "Std. Error"]
)

# Plot the coefficients with ggplot2
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "Predictors", y = "Estimates") +
  theme_minimal()

# Residual Plot
# Extract residuals and fitted values
residuals_zip <- residuals(zip_model, type = "pearson")
fitted_zip <- fitted(zip_model)

# Create residual plot
ggplot(data.frame(fitted = fitted_zip, residuals = residuals_zip), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Fitted values", y = "Residuals") +
  theme_minimal()

# Effect Plot
# Create effect plots
plot(allEffects(zip_model))
