# Install nicheROVER package
install.packages("nicheROVER")
install.packages("tidyverse")
install.packages("Ostats")
install.packages("vegan")
install.packages("tidyverse")
# Load required libraries
library(nicheROVER)
library(ggplot2)
library(unique_overlap)
library(Ostats)
library(vegan)
library(tidyverse)

ant_matrix <- matrix(c(
  0, 0, 26, 18, 0,
  1, 0, 0, 0, 0,
  65, 0, 90, 16, 0,
  0, 4, 73, 91, 4,
  0, 0, 11, 0, 0,
  1, 0, 0, 0, 0,
  0, 57, 34, 64, 57,
  0, 1, 0, 0, 1,
  0, 1, 0, 0, 1,
  2, 0, 0, 0, 0,
  5, 0, 0, 0, 0,
  2, 0, 53, 61, 0,
  0, 0, 3, 0, 0,
  0, 2, 33, 54, 2,
  0, 0, 1, 0, 0,
  0, 0, 0, 0, 0
), nrow = 16, ncol = 5, byrow = TRUE, dimnames = list(
  c("Brachymyrmex_heeri", "Camponotus_macilentus", "Camponotus_planus", "Camponotus_zonatus", "Crematogaster_curvispinosa", "Dorymyrmex_pyramicus", "Monomorium_floricola", "Monomorium_sp.", "Nylanderia_sp.", "Nylanderia_sp1", "Nylanderia_sp3", "Paratrechina_longicornis", "Solenopsis_germinata", "Tapinoma_melanocephalum", "Tetramorium_bicarinatum", "Ant16"),
  c("Fernandina", "Pinta", "San_Cristobal", "Santa_Cruz", "Santiago")
))



# Ensure the data is numeric
ant_matrix <- apply(ant_matrix, 2, as.numeric)

# Define the Sørensen-Dice Index function
sorensen_dice_index <- function(x, y) {
  # Convert x and y to logical vectors
  x <- as.logical(x)
  y <- as.logical(y)
  intersection <- sum(x & y)
  total <- sum(x) + sum(y)
  return(2 * intersection / total)
}

# Initialize the Sørensen-Dice Index matrix
sorensen_dice_matrix <- matrix(0, ncol = ncol(ant_matrix), nrow = ncol(ant_matrix))

# Compute the Sørensen-Dice Index for each pair of columns
for (i in 1:(ncol(ant_matrix) - 1)) {
  for (j in (i + 1):ncol(ant_matrix)) {
    sorensen_dice_matrix[i, j] <- sorensen_dice_index(ant_matrix[, i], ant_matrix[, j])
    sorensen_dice_matrix[j, i] <- sorensen_dice_matrix[i, j] # since it's symmetric
  }
}
# Print the result
print(sorensen_dice_matrix)

# Install and load the necessary packages
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
# Convert the Sørensen-Dice Index matrix to a data frame for plotting
sorensen_dice_df <- melt(sorensen_dice_matrix)

# Rename the columns for better readability
colnames(sorensen_dice_df) <- c("Var1", "Var2", "value")

# Create the heatmap
ggplot(sorensen_dice_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Sørensen-Dice Index") +
  labs(title = "Heatmap of Sørensen-Dice Index",
       x = "Column Index",
       y = "Column Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check the number of columns in ant_matrix
num_columns <- ncol(ant_matrix)
print(num_columns)

str(ant_matrix)




# Assuming ant_matrix has 5 columns, if not, adjust the names accordingly
island_names <- c("Santiago", "Fernandina", "Pinta", "Santa_Cruz", "San_Cristobal")







































































###########
# Function to calculate Sørensen-Dice index
sorensen_dice_index <- function(x, y) {
  2 * sum(pmin(x, y)) / (sum(x) + sum(y))
}

# Calculate pairwise Sørensen-Dice index for niche overlap
sorensen_dice_matrix <- matrix(NA, nrow = ncol(ant_matrix), ncol = ncol(ant_matrix))
rownames(sorensen_dice_matrix) <- colnames(ant_matrix)
colnames(sorensen_dice_matrix) <- colnames(ant_matrix)

for (i in 1:(ncol(ant_matrix) - 1)) {
  for (j in (i + 1):ncol(ant_matrix)) {
    sorensen_dice_matrix[i, j] <- sorensen_dice_index(ant_matrix[, i], ant_matrix[, j])
    sorensen_dice_matrix[j, i] <- sorensen_dice_matrix[i, j] # since it's symmetric
  }
}

# Print the Sørensen-Dice index matrix
print("Sørensen-Dice Index Matrix:")
print(sorensen_dice_matrix)

# Visualize the results with a heatmap
heatmap_plot_sorensen_dice <- ggplot(as.data.frame(sorensen_dice_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Pairwise Sørensen-Dice Index Heatmap",
    x = "Ant Species",
    y = "Ant Species",
    fill = "Sørensen-Dice Index"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_plot_sorensen_dice)

#########
# Convert Hellinger Distance matrix to long format
hellinger_df <- as.data.frame(as.table(hellinger_matrix))
colnames(hellinger_df) <- c("Ant_Species_1", "Ant_Species_2", "Hellinger_Distance")

# Create heatmap plot for Hellinger Distance
heatmap_plot_hellinger <- ggplot(hellinger_df, aes(x = Ant_Species_1, y = Ant_Species_2, fill = Hellinger_Distance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Pairwise Hellinger Distance Heatmap",
    x = "Ant Species",
    y = "Ant Species",
    fill = "Hellinger Distance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_plot_hellinger)

#########
# Convert Sørensen-Dice Index matrix to long format
sorensen_dice_df <- as.data.frame(as.table(sorensen_dice_matrix))
colnames(sorensen_dice_df) <- c("Ant_Species_1", "Ant_Species_2", "Sorensen_Dice_Index")

# Create heatmap plot for Sørensen-Dice Index
heatmap_plot_sorensen_dice <- ggplot(sorensen_dice_df, aes(x = Ant_Species_1, y = Ant_Species_2, fill = Sorensen_Dice_Index)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Pairwise Sørensen-Dice Index Heatmap",
    x = "Ant Species",
    y = "Ant Species",
    fill = "Sørensen-Dice Index"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_plot_sorensen_dice)

###########
# Print Hellinger Distance results
print("Hellinger Distance Results:")
print(hellinger_df)

# Export Hellinger Distance results to CSV
write.csv(hellinger_df, "hellinger_distance_results.csv", row.names = FALSE)

# Print Sørensen-Dice Index results
print("Sørensen-Dice Index Results:")
print(sorensen_dice_df)

# Export Sørensen-Dice Index results to CSV
write.csv(sorensen_dice_df, "sorensen_dice_index_results.csv", row.names = FALSE)

#############
# Function to calculate Hellinger distance
hellinger_distance <- function(x, y) {
  sqrt(sum((sqrt(x) - sqrt(y))^2)) / sqrt(2)
}
# Calculate pairwise Hellinger distance for niche overlap
hellinger_matrix <- matrix(NA, nrow = ncol(ant_matrix), ncol = ncol(ant_matrix))
rownames(hellinger_matrix) <- colnames(ant_matrix)
colnames(hellinger_matrix) <- colnames(ant_matrix)

for (i in 1:(ncol(ant_matrix) - 1)) {
  for (j in (i + 1):ncol(ant_matrix)) {
    hellinger_matrix[i, j] <- hellinger_distance(ant_matrix[, i], ant_matrix[, j])
    hellinger_matrix[j, i] <- hellinger_matrix[i, j] # since it's symmetric
  }
}

# Print the Hellinger distance matrix
print("Hellinger Distance Matrix:")
print(hellinger_matrix)

# Visualize the results with a heatmap
heatmap_plot_hellinger <- ggplot(as.data.frame(hellinger_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Pairwise Hellinger Distance Heatmap",
    x = "Ant Species",
    y = "Ant Species",
    fill = "Hellinger Distance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_plot_hellinger)