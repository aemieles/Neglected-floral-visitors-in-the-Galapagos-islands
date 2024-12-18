install.packages("ggplot2")
library(ggplot2)
setwd("~/IMEDEA/DEPICT/Galapagos/DATOS/Ants/Neglected-floral-visitors-in-the-Galapagos-islands")
ants <- read.csv("Ants.csv")
View(ants)
ggplot(ants, aes(x=Status, y=SpeciesStrength, fill=Status)) +
  geom_boxplot() +
  labs(title="Box and Whisker Plot of SpeciesStrength by Status",
       x="Status",
       y="SpeciesStrength (Ants)") +
  theme_minimal() +
  scale_fill_manual(values=c("blue", "red"))

# Perform t-test
t_test_result <- t.test(SpeciesStrength ~ Status, data = ants)

# Print the t-test result
print(t_test_result)

# Extract the t-value
t_value <- t_test_result$statistic
cat("The t-value is:", t_value, "\n")

# Box and Whisker for plants
plants <- read.csv("Plants.csv")
View(plants)
ggplot(plants, aes(x=Status, y=SpeciesStrength, fill=Status)) +
  geom_boxplot() +
  labs(title="Box and Whisker Plot of SpeciesStrength by Status",
       x="Status",
       y="SpeciesStrength (plants)") +
  theme_minimal() +
  scale_fill_manual(values=c("blue", "red", "green"))

# Perform t-test
t_test_result <- t.test(SpeciesStrength ~ Status, data = plants)

# Print the t-test result
print(t_test_result)

# Extract the t-value
t_value <- t_test_result$statistic
cat("The t-value is:", t_value, "\n")

#=======================#
# Create the boxplot
ggplot(plants, aes(x=Status, y=SpeciesStrength, fill=Status)) +
  geom_boxplot() +
  labs(title="Box and Whisker Plot of SpeciesStrength by Status",
       x="Status",
       y="SpeciesStrength (plants)") +
  theme_minimal() +
  scale_fill_manual(values=c("blue", "red")) + # Use appropriate colors
  stat_summary(fun = median, geom = "point", size = 2, color = "yellow") + # Adding median points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") # Adding error bars


1
